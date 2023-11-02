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

(defvar vt-thread-list (vt-read-json-from-url vt-url))

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

;; (no . 1)
;; (no . 61810952)
;; (no . 61812609)
;; (no . 61809485)
;; (no . 61808592)
;; (no . 61766439)
;; (no . 61772252)
;; (no . 61795378)
;; (no . 61764122)
;; (no . 61806139)
;; (no . 61795376)
;; (no . 61794133)
;; (no . 61736127)
;; (no . 61812607)
;; (no . 61813121)
;; (no . 61783642)
;; (no . 61805253)
;; (no . 61799646)
;; (no . 61757001)
;; (no . 61812371)
;; (no . 61806048)
;; (no . 61629692)
;; (no . 61803336)
;; (no . 61769721)
;; (no . 61781654)
;; (no . 61765866)
;; (no . 61550008)
;; (no . 61789389)
;; (no . 61769174)
;; (no . 61796736)
;; (no . 61709750)
;; (no . 61784471)
;; (no . 61796847)
;; (no . 61394971)
;; (no . 61809774)
;; (no . 61758450)
;; (no . 61801128)
;; (no . 61797660)
;; (no . 61790767)
;; (no . 61801395)
;; (no . 61729154)
;; (no . 61798094)
;; (no . 61812050)
;; (no . 61809580)
;; (no . 61807614)
;; (no . 61768442)
;; (no . 61662384)
;; (no . 61767489)
;; (no . 61757051)
;; (no . 61681655)
;; (no . 61763579)
;; (no . 61709104)
;; (no . 61797297)
;; (no . 61673114)
;; (no . 61809958)
;; (no . 61790486)
;; (no . 61597749)
;; (no . 61413084)
;; (no . 61599846)
;; (no . 61801196)
;; (no . 61641824)
;; (no . 61647367)
;; (no . 61716217)
;; (no . 61785927)
;; (no . 61806490)
;; (no . 61451286)
;; (no . 61422520)
;; (no . 61805992)
;; (no . 61775932)
;; (no . 61735067)
;; (no . 61678494)
;; (no . 61774508)
;; (no . 61795532)
;; (no . 61802065)
;; (no . 61812121)
;; (no . 61744486)
;; (no . 61496902)
;; (no . 61581707)
;; (no . 61480929)
;; (no . 61695038)
;; (no . 61811313)
;; (no . 61769781)
;; (no . 61724457)
;; (no . 61807160)
;; (no . 61609843)
;; (no . 61808989)
;; (no . 61731935)
;; (no . 61506698)
;; (no . 61770886)
;; (no . 61741615)
;; (no . 61694321)
;; (no . 61799529)
;; (no . 61789069)
;; (no . 61809882)
;; (no . 61716247)
;; (no . 61804889)
;; (no . 61786207)
;; (no . 61804936)
;; (no . 61708442)
;; (no . 61808913)
;; (no . 61789881)
;; (no . 61461169)
;; (no . 61633826)
;; (no . 61743788)
;; (no . 61786893)
;; (no . 61776804)
;; (no . 61791578)
;; (no . 61664508)
;; (no . 61798155)
;; (no . 61804216)
;; (no . 61812271)
;; (no . 61688288)
;; (no . 61810741)
;; (no . 61505437)
;; (no . 61704695)
;; (no . 61810853)
;; (no . 61604957)
;; (no . 61796975)
;; (no . 61657624)
;; (no . 61793078)
;; (no . 61809122)
;; (no . 61496060)
;; (no . 61617166)
;; (no . 61554323)
;; (no . 61763302)
;; (no . 61790066)
;; (no . 61618614)
;; (no . 61799032)
;; (no . 61738031)
;; (no . 61768145)
;; (no . 61789671)
;; (no . 61727393)
;; (no . 61789627)
;; (no . 61419625)
;; (no . 61761568)
;; (no . 61573098)
;; (no . 61804257)
;; (no . 61728466)
;; (no . 61807627)
;; (no . 61792690)
;; (no . 61805935)
;; (no . 61808984)
;; (no . 61591904)
;; (no . 61801574)
;; (no . 61799313)
;; (no . 61776853)
;; (no . 61793813)
;; (no . 61701793)
;; (no . 61810926)
;; (no . 61788511)
;; (no . 61793192)

(defvar vt-post-ids '())

(vt-with-thread vt-thread-list
                (lambda (thread) (setq! vt-post-ids (cons (cdr (assoc 'no thread)) vt-post-ids))))


(defun vt-with-post (post-list go)
  "Call GO for each post in POST-VEC."
  (let ((posts (cdar post-list)))
    (dotimes (index (length posts))
      (funcall go (aref posts index)))))


(defvar thread-id "61793192")
(defvar vt-thread-url (concat "https://a.4cdn.org/vt/thread/" thread-id ".json"))


(defvar vt-post-list (vt-read-json-from-url vt-thread-url))

(defvar vt/temp-buf (get-buffer-create "*posts*"))
(vt-with-post vt-post-list (lambda (post)
                             (with-current-buffer vt/temp-buf
                               (insert (vt-post-to-markdown post)))))


;; (cdr (assoc 'sub (aref (cdar vt-post-list) 0)))

(defun vt-post-to-markdown (post)
  "Convert POST to markdown."
  (let ((title (cdr (assoc 'sub post)))
        (body (cdr (assoc 'com post)))
        (no (cdr (assoc 'no post)))
        (is-op (zerop (cdr (assoc 'resto post)))))
    (if is-op
        (format "# %s -- (%s)\n\n%s\n\n" title no body)
      (format "## %s -- (%s)\n\n%s\n\n" title no body))))



;; (with-current-buffer (get-buffer-create "*test*")
;;   (insert (vt-post-to-markdown (aref (cdar vt-post-list) 0))))

(provide 'vt)
;;; vt.el ends here
