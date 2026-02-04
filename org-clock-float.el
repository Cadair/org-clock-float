;;; ...  -*- lexical-binding: t -*-
;;; org-clock-float.el --- Summary

;; Copyright (C) 2025 Stuart Mumford

;; Author: Stuart Mumford <stuart@cadair.com>
;; Keywords: org, clocking, float
;; Version: 1.1
;; Package-Requires: ((emacs "26.1") (request "0.3.2") (org "9"))
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
If cached, invoke CALLBACK immediately.

The CALLBACK is called with a list of project alists as returned by
the Float API."
  (if float--projects-cache
      (funcall callback float--projects-cache)
    (request
      (concat org-clock-float-api-base-url "projects")
      :type "GET"
      :headers (org-clock-float--build-headers headers)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  ;; Cache the raw project objects so we can look them up
                  ;; by different keys (name, project_code, project_id).
                  (setq float--projects-cache data)
                  (funcall callback data)))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (when error-callback
                  (funcall error-callback error-thrown)))))))

(defun float-get-project-by-name-async (project-name callback &optional headers error-callback)
  "Asynchronously get a project by PROJECT-NAME, using cache.

This preserves the legacy behaviour used by `float_' tags, which
refer to the project's `name'."
  (let* ((find-project
          (lambda (projects)
            (cl-find-if
             (lambda (project)
               (let ((name (cdr (assoc 'name project))))
                 (and name (string= name project-name))))
             projects))))
    (let ((maybe-return (when float--projects-cache
                          (funcall find-project float--projects-cache))))
      (if maybe-return
          (funcall callback maybe-return)
        (float-get-projects-async
         (lambda (projects)
           (funcall callback (funcall find-project projects)))
         headers
         error-callback)))))


(defun float-get-active-phase-for-project-async (project-id callback &optional headers error-callback)
  "Asynchronously get the single active phase for PROJECT-ID.

CALLBACK is called with either the active phase alist (when exactly
one active phase is found) or nil (when no active phases exist).

If more than one active phase is returned from the API, this is
treated as an error condition: a message is emitted and
ERROR-CALLBACK (if non-nil) is invoked. In that case, CALLBACK is
*not* called."
  (request
    (concat org-clock-float-api-base-url "phases")
    :type "GET"
    :params `(("project_id" . ,project-id)
              ("active" . "1"))
    :headers (org-clock-float--build-headers headers)
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                ;; Normalise DATA into a plain list of phase alists.
                (let* ((phases
                        (mapcar (lambda (phase)
                                  (cond
                                   ;; Common case: a single-element vector wrapping the alist
                                   ((and (vectorp phase)
                                         (= (length phase) 1)
                                         (listp (aref phase 0)))
                                    (aref phase 0))
                                   ;; Already an alist
                                   ((listp phase) phase)
                                   ;; Fallback: return as-is
                                   (t phase)))
                                (append data nil)))
                       (active-phases
                        (cl-remove-if-not
                         (lambda (phase)
                           (let ((active (cdr (assoc 'active phase))))
                             (eq active 1)))
                         phases))
                       (count (length active-phases)))
                  (cond
                   ((= count 0)
                    ;; No active phases: just report nil to the caller.
                    (funcall callback nil))
                   ((= count 1)
                    ;; Exactly one active phase: already normalised above.
                    (funcall callback (car active-phases)))
                   (t
                    ;; More than one active phase: error.
                    (message "org-clock-float: multiple active phases found for project_id %s; not logging time" project-id)
                    (when error-callback
                      (funcall error-callback
                               (format "multiple active phases for project_id %s" project-id))))))))
    :error (cl-function
            (lambda (&rest _ &key error-thrown &allow-other-keys)
              (message "org-clock-float: failed to fetch phases for project_id %s: %S" project-id error-thrown)
              (when error-callback
                (funcall error-callback error-thrown))))))


(defun float-get-project-by-code-async (project-code callback &optional headers error-callback)
  "Asynchronously get a project by PROJECT-CODE, using cache.

This is intended for use with `floatid_' tags, which refer to the
project's `project_code' in Float."
  (let* ((find-project
          (lambda (projects)
            (cl-find-if
             (lambda (project)
               (let ((code (cdr (assoc 'project_code project))))
                 (and code (string= code project-code))))
             projects))))
    (let ((maybe-return (when float--projects-cache
                          (funcall find-project float--projects-cache))))
      (if maybe-return
          (funcall callback maybe-return)
        (float-get-projects-async
         (lambda (projects)
           (funcall callback (funcall find-project projects)))
         headers
         error-callback)))))


(defun org-clock-float-post-task ()
  "clock out post the clock to Float.

This function performs an asynchronous sequence:
1. Validate and extract the Float project tag from the current Org entry.
2. Resolve the current user (people_id) from Float via email (cached).
3. Resolve the project (project_id) from Float via tag-derived identifier
   (either project name via `float_' tags or project_code via `floatid_' tags).
4. Resolve the single active phase for the project, if any.
5. POST the logged time to Float, optionally associated with that phase.

Each network step is non-blocking and handled via callbacks.
Errors at any step are surfaced via `message`."
  (interactive)
  ;; Gather local context for the log entry (no network here).
  (let* ((tags (org-get-tags))
         (title (org-entry-get nil "ITEM"))
         (clocked-time (org-clock-float--get-last-clock-duration))
         (clocked-timestamp (org-clock-float--get-last-clock-timestamp))
         (todays-date (org-timestamp-format clocked-timestamp "%Y-%m-%d" t))
         (float-name-tags (cl-remove-if-not (lambda (ele) (string-prefix-p "float_" ele)) tags))
         (float-id-tags (cl-remove-if-not (lambda (ele) (string-prefix-p "floatid_" ele)) tags)))
    ;; Step 1: Validate presence of a Float project tag and derive project identifier.
    ;; Prefer `floatid_' tags (by project_code / project_id) when present,
    ;; but keep supporting legacy `float_' tags which match project names.
    (if (and (null float-name-tags) (null float-id-tags))
        (message "org-clock-float: no float_ or floatid_ tag found on task; skipping post")
      (let* ((using-id-tag (not (null float-id-tags)))
             (raw-tag (if using-id-tag
                          (car float-id-tags)
                        (car float-name-tags)))
             (project-identifier
              (if using-id-tag
                  ;; `floatid_' tags: use the remainder verbatim as project_code.
                  (substring raw-tag (length "floatid_"))
                ;; Legacy `float_' tags: underscores represent spaces in the project name.
                (string-replace "_" " " (substring raw-tag (length "float_"))))))
        ;; Step 2: Resolve person (people_id) asynchronously using cached directory.
        (float-get-person-async
         org-clock-float-email
         (lambda (person)
           (let ((people-id (cdr (assoc 'people_id person))))
             (if (null people-id)
                 (message "org-clock-float: could not resolve person id for %s" org-clock-float-email)
               ;; Step 3: Resolve project (project_id) asynchronously using cached list.
               (funcall
                (if using-id-tag
                    #'float-get-project-by-code-async
                  #'float-get-project-by-name-async)
                project-identifier
                (lambda (project)
                  (let ((project-id (cdr (assoc 'project_id project))))
                    (if (null project-id)
                        (message "org-clock-float: could not resolve project id for %s" project-identifier)
                      ;; Step 4: Resolve the active phase (if any) for this project.
                      (float-get-active-phase-for-project-async
                       project-id
                       (lambda (phase)
                         (let* ((phase-id (and phase (cdr (assoc 'phase_id phase)))))
                           (message "org-clock-float: phase lookup for project_id %s returned phase_id=%S, raw=%S"
                                    project-id phase-id phase)
                           (let* ((payload
                                   `(("people_id" . ,people-id)
                                     ("date" . ,todays-date)
                                     ("hours" . ,clocked-time)
                                     ("project_id" . ,project-id)
                                     ("task_name" . ,title)
                                     ,@(when phase-id
                                         `(("phase_id" . ,phase-id))))))
                             ;; Step 5: POST the time entry asynchronously to Float.
                             (float-make-post
                              (concat org-clock-float-api-base-url "logged-time")
                              payload
                              nil
                              ;; Success callback for POST
                              (cl-function
                               (lambda (&key data &allow-other-keys)
                                 (if phase-id
                                     (message "org-clock-float: logged %.2fh to %s (phase id %s)"
                                              clocked-time project-identifier phase-id)
                                   (message "org-clock-float: logged %.2fh to %s (no active phase)"
                                            clocked-time project-identifier))))
                              ;; Error callback for POST
                              (cl-function
                               (lambda (&rest _ &key error-thrown &allow-other-keys)
                                 (message "org-clock-float: failed to post time: %S" error-thrown)))))))
                       ;; Error callback for phase lookup
                       (lambda (_err)
                         (message "org-clock-float: phase lookup failed for project_id %s; not logging time"
                                  project-id))))))
                nil
                ;; Error callback for project lookup
                (lambda (_err)
                  (message "org-clock-float: project lookup failed for %s" project-identifier))))))
         nil
         ;; Error callback for people lookup
         (lambda (_err)
           (message "org-clock-float: people lookup failed for %s" org-clock-float-email)))))))


(defun org-clock-float-setup ()
  "Setup org-clock-float."
  (message "Setting up org-clock-float")
  (add-hook 'org-clock-out-hook #'org-clock-float-post-task))

(provide 'org-clock-float)

;;; org-float-integration.el ends here
