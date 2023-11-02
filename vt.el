;;; vt.el --- Browse /vt/ -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Yaxley Peaks
;;
;; Author: Yaxley Peaks <epiclycoolgaemer@gmail.com>
;; Maintainer: Yaxley Peaks <epiclycoolgaemer@gmail.com>
;; Created: November 02, 2023
;; Modified: November 02, 2023
;; Version: 0.0.1
;; Keywords: convenience
;; Homepage: https://github.com/yaxley-peaks/vt
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'json)


;; Hardcode /vt/ here
;; https://github.com/4chan/4chan-API/blob/master/pages/Threadlist.md
;; List of all threads in board
(defvar vt-url "https://a.4cdn.org/vt/threads.json")

(defun vt-read-json-from-url (url)
  "Get json from url and return it"
  (with-current-buffer
      (url-retrieve-synchronously url)
    (goto-char (point-min))
    (search-forward "\n\n")
    (json-read)))

(defvar vt-thread-list (vt-read-json-from-url vt-url))

;; (with-current-buffer (get-buffer-create "*test*") (insert (format "%s"  (aref vt-thread-list 0))))

;; (setq page-number 0 thread-number 2)
;; (assoc 'no (aref
;;             (cdr (assoc 'threads (aref vt-thread-list page-number))) thread-number))

(defun vt-with-thread (thread-vec go)
  "Call go for each thread in thread-vec"
  (dotimes (index (length thread-vec))
    (let ((v-threads-on-page (cdr (assoc 'threads (aref vt-thread-list index)))))
      (dotimes (thread-index (length v-threads-on-page))
        (let ((thread (aref v-threads-on-page thread-index))) (funcall go thread))))))

;; (vt-get-thread-nos vt-thread-list (lambda (x) (insert (format "%s\n" (assoc 'no x)))))


(provide 'vt)
;;; vt.el ends here
