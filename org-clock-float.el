;;; ...  -*- lexical-binding: t -*-
;;; org-clock-float.el --- Summary

;; Copyright (C) 2025 Stuart Mumford

;; Author: Stuart Mumford <stuart@cadair.com>
;; Keywords: org, clocking, float
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (request "0.3.2")
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

;; Float Integration
(defun org-clock-float--get-last-clock-duration ()
  "Get the last clock entry for the current task."
  (/ (org-duration-to-minutes (org-element-property :duration (org-element-at-point))) 60)
)

(defun org-clock-float--get-last-clock-timestamp ()
  "Get the last clock entry for the current task."
  (org-element-property :value (org-element-at-point))
)

(defun float-make-post (url data &optional headers)
  (add-to-list 'headers `("Content-Type" . "application/json"))
  (add-to-list 'headers `("Accept" . "application/json"))
  (add-to-list 'headers org-clock-float-api-auth-header)
  ;; (add-to-list 'headers `("User-Agent" . ,(concat "Emacs " org-clock-float-email)))
  (request
    url
    :type "POST"
    :data (json-encode data)
    :headers headers
    :parser 'json-read
    )
  )

(defun float-get-people (&optional headers)
  "Get all people from the Float API."
  (add-to-list 'headers org-clock-float-api-auth-header)
  ;; (add-to-list 'headers `("User-Agent" . ,(concat "Emacs " org-clock-float-email)))
  (request-response-data
   (request
     (concat org-clock-float-api-base-url "people")
     :type "GET"
     :sync t
     :parser 'json-read
     :headers headers
     )
   )
  )

(defun float-get-person (email &optional headers)
  "Get information about a person based on their email."
  (let* ((people (float-get-people headers))
         (person (elt (cl-remove-if-not (lambda (row) (equal (cdr (assoc 'email row)) email)) people) 0)))
    person
    )
  )


(defun float-get-projects (&optional headers)
  "Get all projects from the Float API"
  (add-to-list 'headers org-clock-float-api-auth-header)
  ;; (add-to-list 'headers `("User-Agent" . ,(concat "Emacs " org-clock-float-email)))
  (request-response-data
   (request
     (concat org-clock-float-api-base-url "projects")
     :type "GET"
     :sync t
     :parser 'json-read
     :headers headers
     )
   )
  )

(defun float-get-project (project_name &optional headers)
  "Get information about a project based on it's name."
  (let ((projects (float-get-projects)))
	;; TODO: This errors if there isn't a matching float tag on a task i.e. if there's nothing to be passed to the elt
	;; elt: Wrong type argument: stringp, nil
    (elt (cl-remove-if-not (lambda (prj) (equal (cdr (assoc 'name prj)) project_name)) projects) 0)
    )
  )

(defun org-clock-float-post-task ()
  "clock out post the clock to Float."
  (interactive)
  (let* ((tags (org-get-tags))
         (title (org-entry-get nil "ITEM"))
         (clocked-time (org-clock-float--get-last-clock-duration))
         (clocked-timestamp (org-clock-float--get-last-clock-timestamp))
         (todays-date (org-timestamp-format clocked-timestamp "%Y-%m-%d" t))
         (people_id (cdr (assoc 'people_id (float-get-person org-clock-float-email))))
         (float-tags (cl-remove-if-not (lambda (ele) (string-match "float_" ele)) tags))
         (project_name (string-replace "_" " " (elt (split-string (elt float-tags 0) "float_") 1)))
         (project_id (cdr (assoc 'project_id (float-get-project project_name))))
         )

    (float-make-post
     (concat org-clock-float-api-base-url "logged-time")
     `(("people_id" . ,people_id)
       ("date" . ,todays-date)
       ("hours" . ,clocked-time)
       ("project_id" . ,project_id)
       ("task_name" . ,title)))
    )
  )


(defun org-clock-float-setup ()
  "Setup org-clock-float."
  (message "Setting up org-clock-float")
  (add-hook 'org-clock-out-hook #'org-clock-float-post-task))

(provide 'org-clock-float)

;;; org-float-integration.el ends here
