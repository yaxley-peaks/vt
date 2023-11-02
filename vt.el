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
  "Get json from URL and return it."
  (with-current-buffer
      (url-retrieve-synchronously url)
    (goto-char (point-min))
    (search-forward "\n\n")
    (json-read)))


;;
;; (setq page-number 0 thread-number 2)
;; (assoc 'no (aref
;;             (cdr (assoc 'threads (aref vt-thread-list page-number))) thread-number))

(defun vt-with-thread (thread-vec go)
  "Call GO for each thread in THREAD-VEC."
  (dotimes (index (length thread-vec))
    (let ((v-threads-on-page (cdr (assoc 'threads (aref vt-thread-list index)))))
      (dotimes (thread-index (length v-threads-on-page))
        (let ((thread (aref v-threads-on-page thread-index))) (funcall go thread))))))

;; (vt-with-thread vt-thread-list (lambda (x) (insert (format "%s\n" (assoc 'no x)))))

(defun vt-with-post (post-list go)
  "Call GO for each post in POST-VEC."
  (let ((posts (cdar post-list)))
    (dotimes (index (length posts))
      (funcall go (aref posts index)))))

(defun vt-post-to-markdown (post)
  "Convert POST to markdown."
  (let ((title (cdr (assoc 'sub post)))
        (body (cdr (assoc 'com post)))
        (no (cdr (assoc 'no post)))
        (is-op (zerop (cdr (assoc 'resto post)))))
    (if is-op
        (format "# %s -- (%s)\n\n%s\n\n" title no body)
      (format "## %s -- (%s)\n\n%s\n\n" title no body))))

;; (defvar thread-id "61793192")
;; (defvar vt-thread-url (concat "https://a.4cdn.org/vt/thread/" thread-id ".json"))


;; (defvar vt-post-list (vt-read-json-from-url vt-thread-url))

;; (defvar vt/temp-buf (get-buffer-create "*posts*"))
;; (vt-with-post vt-post-list (lambda (post)
;;                              (with-current-buffer vt/temp-buf
;;                                (insert (vt-post-to-markdown post)))))


;;;  Things left to do:
;;;  1. Image support
;;;  2. Show something else instead of nil in titles
;;;  3. Prettier titles
;;;  4. More info in headers. Tripcodes, etc.

(defvar vt-thread-list (vt-read-json-from-url vt-url))
(defvar vt-post-ids '())
(vt-with-thread vt-thread-list
                (lambda (thread) (setq! vt-post-ids (cons (cdr (assoc 'no thread)) vt-post-ids))))
(defun vt-all-threads-to-markdown (thread-id-list)
  "THREAD-ID-LIST is a list of integers"
  (let ((temp-buffer (get-buffer-create "*vt-posts*"))
        (posts-url "https://a.4cdn.org/vt/thread/"))
    (dolist (post-id thread-id-list)
      (let ((url (concat posts-url (number-to-string post-id) ".json")))
        (with-current-buffer temp-buffer (vt-with-post (vt-read-json-from-url url)
                                                       (lambda (post) (insert (vt-post-to-markdown post)))))))))




;; (cdr (assoc 'sub (aref (cdar vt-post-list) 0)))


;; (vt-all-threads-to-markdown vt-post-ids)


;; (with-current-buffer (get-buffer-create "*test*")
;;   (insert (vt-post-to-markdown (aref (cdar vt-post-list) 0))))

(provide 'vt)
;;; vt.el ends here
