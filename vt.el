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
  "Get json from url"
  (with-current-buffer
      (url-retrieve-synchronously url)
    (goto-char (point-min))
    (search-forward "\n\n")
    (json-read)))



(provide 'vt)
;;; vt.el ends here
