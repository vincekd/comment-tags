;;; comment-tags.el --- Highlight and navigate comment tags like TODO, FIXME, etc -*- lexical-binding: t -*-

;; Copyright © 2017 Vincent Dumas <vincekd@gmail.com>

;; Author: Vincent Dumas <vincekd@gmail.com>
;; URL: https://github.com/vincekd/comment-tags
;; Keywords: project, convenience, comments, tags
;; Version: 0.1
;; Package-Requires: ((emacs "24.1") (pkg-info "0.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; TODO: finish this
;;
;;; Code:

;;(require 'rx)
;;(require 'cl-lib)

;; TODO: make minor mode
;; (defun my-highlight-prog-words ()
;;   (let* ((comment-opener `(or "//" "/*"))
;;         (prog-specials
;;          (rx (eval comment-opener) (0+ whitespace) (group (or "FIXME" "TODO" "BUG") ":"))
;;          ;;(rx (syntax comment-start) (0+ whitespace) (group (or "FIXME" "TODO" "BUG") ":"))
;;          ;;(rx word-start (group (or "FIXME" "TODO" "BUG") ":"))
;;          ))
;;     (font-lock-add-keywords nil
;;                             `((,prog-specials 1 ',font-lock-warning-face t)))))
;; (add-hook 'prog-mode-hook 'my-highlight-prog-words)

;;; customize
(defgroup comment-tags nil
  "Highlight and navigate comment tags."
  :group 'tools
  :link '(url-link :tag "Github" "https//github.com/vincekd/comment-tags"))

(defcustom comment-tags/keywords
  '("TODO"
    "FIXME"
    "BUG"
    "HACK"
    "XXX")
  "Keywords to highlight and track."
  :group 'comment-tags
  :type '(repeat string))

(defcustom comment-tags/require-colon t
  "Require colon after tags."
  :group 'comment-tags
  :type 'boolean)

(defcustom comment-tags/comment-start nil
  "Only highlight and track tags that are the beginning of a comment"
  :group 'comment-tags
  :type 'boolean)

(defcustom font-lock-comment-tags-face 'font-lock-comment-tags-face
  "Face for highlighting tags."
  :group 'comment-tags)

(defcustom comment-tags/foreground-color "Red"
  "Font foreground color"
  :group 'comment-tags
  :type 'string)
(defcustom comment-tags/background-color nil
  "Font background color"
  :group 'comment-tags
  :type 'string)

;;(make-face 'font-lock-comment-tags-face)
;; (modify-face 'font-lock-comment-tags-face
;;              comment-tags/foreground-color
;;              comment-tags/background-color
;;              nil t nil t nil nil)
(defface comment-tags/face
  '((t :foreground "red"
       :background nil
       :weight bold
       :underline nil))
  "Font face for highlighted tags."
  :group 'comment-tags)

(defvar comment-tags/syntax-table
  (let ((st (make-syntax-table)))
    (message "make syntax-table")))

(defun comment-tags--join (list joiner)
  (mapconcat 'identity list joiner))

(defun comment-tags/make-regexp ()
  (concat "\\<\\(\\(?:" (comment-tags--join comment-tags/keywords "\\|") "\\)"
          (if comment-tags/require-colon
              ":"
            "") "\\)"))

(defconst comment-tags/syntax-propertize-function
  (syntax-propertize-rules
   ((comment-tags/make-regexp)
    (0 (comment-tags/highlight-keywords)))))

(defun comment-tags/highlight-keywords ()
  "Highlight tags in var `comment-tags/keywords'."
  (save-excursion
    (let* ((end-p (point))
           (start-p (match-beginning 0))
           (in-comment (and (nth 4 (syntax-ppss start-p)) (nth 4 (syntax-ppss end-p)))))
      (when in-comment
        (message "be: %s; end: %s; str: %s" start-p end-p (match-string-no-properties 0))
        ;;(set-text-properties start-p end-p nil)
        ;;(put-text-property start-p end-p 'face (list :foreground "#FF0000"))
        ;;(add-text-properties start-p end-p '(face font-lock-comment-tags-face))
        ;;(put-text-property start-p end-p 'face 'font-lock-comment-tags-face)
        (put-text-property start-p end-p 'face 'comment-tags/face)
        ))))

;;;###autoload
(define-minor-mode comment-tags-mode
  "Highlight and navigate comment tags."
  :lighter "Comment Tags"
  (message "loaded comment-tags")
  ;;(set (make-local-variable 'comment-tags-function) 'comment-tags/highlight-keywords)
  (set (make-local-variable 'syntax-propertize-function)
       comment-tags/syntax-propertize-function))

(provide 'comment-tags)

;;; comment-tags.el ends here
