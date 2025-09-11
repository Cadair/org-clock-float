;;; ...  -*- lexical-binding: t -*-
;;; org-clock-float.el --- Summary

;; Copyright (C) 2025 Stuart Mumford

;; Author: Stuart Mumford <stuart@cadair.com>
;; Keywords: org, clocking, float
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (request "0.3.2"))
;; URL: https://github.com/Cadair/org-clock-float/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Clock the active task on float.com upon checkout.
;;

;;; Code:
(require 'request)
(require 'json)
(require 'org-clock)
(require 'cl-lib)

(defgroup org-clock-float nil
  "Send clock time to Float on clockout"
  :group 'emacs)


(defcustom org-clock-float-email ""
  "Text to display when not clocked in on any task."
  :type 'string
  :group 'org-clock-float)

(defcustom org-clock-float-api-base-url "https://api.float.com/v3/"
  "Float API base URL."
  :type 'string
  :group 'org-clock-float)

(defcustom org-clock-float-api-token ""
  "Float API auth token"
  :type 'string
  :group 'org-clock-float)

(defcustom org-clock-float-api-auth-header `("Authorization" . ,(concat "Bearer " org-clock-float-api-token))
  "Float Auth Header"
  :type 'list
  :group 'org-clock-float)

(defun org-clock-float--build-headers (&optional extra-headers)
  "Construct default headers merged with EXTRA-HEADERS without mutating inputs."
  (let* ((base (list (cons "Content-Type" "application/json")
                     (cons "Accept" "application/json")
                     (cons "User-Agent" (concat "Emacs " org-clock-float-email))
                     (cons (car org-clock-float-api-auth-header)
                           (concat "Bearer " org-clock-float-api-token)))))
    (append base extra-headers)))

;; Float Integration
(defun org-clock-float--get-last-clock-duration ()
  "Get the last clock entry for the current task."
  (/ (org-duration-to-minutes (org-element-property :duration (org-element-at-point))) 60)
)

(defun org-clock-float--get-last-clock-timestamp ()
  "Get the last clock entry for the current task."
  (org-element-property :value (org-element-at-point))
)

(defun float-make-post (url data &optional headers success error complete)
  (request
    url
    :type "POST"
    :data (json-encode data)
    :headers (org-clock-float--build-headers headers)
    :parser 'json-read
    :success (or success (lambda (&rest _) (message "org-clock-float: POST success")))
    :error (or error (lambda (&rest args &key error-thrown &allow-other-keys)
                       (message "org-clock-float: POST error %S" error-thrown)))
    :complete (or complete (lambda (&rest _) (ignore)))))


(defvar float--people-cache nil "Cache for storing people data as an alist.")


(defun float-get-people-async (callback &optional headers error-callback)
  "Asynchronously fetch people and pass alist to CALLBACK.
If cached, invoke CALLBACK immediately. ERROR-CALLBACK called on failure."
  (if float--people-cache
      (funcall callback float--people-cache)
    (request
      (concat org-clock-float-api-base-url "people")
      :type "GET"
      :headers (org-clock-float--build-headers headers)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((alist (mapcar (lambda (person)
                                         (cons (cdr (assoc 'email person)) person))
                                       data)))
                    (setq float--people-cache alist)
                    (funcall callback alist))))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (when error-callback
                  (funcall error-callback error-thrown)))))))


(defun float-get-person-async (email callback &optional headers error-callback)
  "Asynchronously get a person by EMAIL, using cache. CALLBACK receives cons entry."
  (let ((maybe-return (when (and float--people-cache (assoc email float--people-cache))
                        (assoc email float--people-cache))))
    (if maybe-return
        (funcall callback maybe-return)
      (float-get-people-async
       (lambda (alist)
         (funcall callback (assoc email alist)))
       headers
       error-callback))))


(defvar float--projects-cache nil "Cache for storing projects data as an alist.")


(defun float-get-projects-async (callback &optional headers error-callback)
  "Asynchronously fetch projects and pass alist to CALLBACK.
If cached, invoke CALLBACK immediately."
  (if float--projects-cache
      (funcall callback float--projects-cache)
    (request
      (concat org-clock-float-api-base-url "projects")
      :type "GET"
      :headers (org-clock-float--build-headers headers)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((alist (mapcar (lambda (project)
                                         (cons (cdr (assoc 'name project)) project))
                                       data)))
                    (setq float--projects-cache alist)
                    (funcall callback alist))))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (when error-callback
                  (funcall error-callback error-thrown)))))))

(defun float-get-project-async (project_name callback &optional headers error-callback)
  "Asynchronously get a project by PROJECT_NAME, using cache."
  (let ((maybe-return (when (and float--projects-cache (assoc project_name float--projects-cache))
                        (assoc project_name float--projects-cache))))
    (if maybe-return
        (funcall callback maybe-return)
      (float-get-projects-async
       (lambda (alist)
         (funcall callback (assoc project_name alist)))
       headers
       error-callback))))


(defun org-clock-float-post-task ()
  "clock out post the clock to Float.

This function performs an asynchronous sequence:
1. Validate and extract the Float project tag from the current Org entry.
2. Resolve the current user (people_id) from Float via email (cached).
3. Resolve the project (project_id) from Float via tag-derived name (cached).
4. POST the logged time to Float.

Each network step is non-blocking and handled via callbacks.
Errors at any step are surfaced via `message`."
  (interactive)
  ;; Gather local context for the log entry (no network here).
  (let* ((tags (org-get-tags))
         (title (org-entry-get nil "ITEM"))
         (clocked-time (org-clock-float--get-last-clock-duration))
         (clocked-timestamp (org-clock-float--get-last-clock-timestamp))
         (todays-date (org-timestamp-format clocked-timestamp "%Y-%m-%d" t))
         (float-tags (cl-remove-if-not (lambda (ele) (string-match "float_" ele)) tags))
         )
    ;; Step 1: Validate presence of a Float project tag and derive project name.
    (if (null float-tags)
        (message "org-clock-float: no float_ tag found on task; skipping post")
      (let* ((project-name (string-replace "_" " " (elt (split-string (elt float-tags 0) "float_") 1))))
        ;; Step 2: Resolve person (people_id) asynchronously using cached directory.
        (float-get-person-async
         org-clock-float-email
         (lambda (person)
           (let ((people-id (cdr (assoc 'people_id person))))
             (if (null people-id)
                 (message "org-clock-float: could not resolve person id for %s" org-clock-float-email)
               ;; Step 3: Resolve project (project_id) asynchronously using cached list.
               (float-get-project-async
                project-name
                (lambda (project)
                  (let ((project-id (cdr (assoc 'project_id project))))
                    (if (null project-id)
                        (message "org-clock-float: could not resolve project id for %s" project-name)
                      ;; Step 4: POST the time entry asynchronously to Float.
                      (float-make-post
                       (concat org-clock-float-api-base-url "logged-time")
                       `(("people_id" . ,people-id)
                         ("date" . ,todays-date)
                         ("hours" . ,clocked-time)
                         ("project_id" . ,project-id)
                         ("task_name" . ,title))
                       nil
                       ;; Success callback for POST
                       (cl-function (lambda (&key data &allow-other-keys)
                                     (message "org-clock-float: logged %.2fh to %s" clocked-time project-name)))
                       ;; Error callback for POST
                       (cl-function (lambda (&rest _ &key error-thrown &allow-other-keys)
                                     (message "org-clock-float: failed to post time: %S" error-thrown))))))
                 nil
                 ;; Error callback for project lookup
                 (lambda (_err)
                   (message "org-clock-float: project lookup failed for %s" project-name))))))
          nil
          ;; Error callback for people lookup
          (lambda (_err)
            (message "org-clock-float: people lookup failed for %s" org-clock-float-email))))))))


(defun org-clock-float-setup ()
  "Setup org-clock-float."
  (message "Setting up org-clock-float")
  (add-hook 'org-clock-out-hook #'org-clock-float-post-task))

(provide 'org-clock-float)

;;; org-float-integration.el ends here
