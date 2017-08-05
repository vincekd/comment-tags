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
;;; TODO:
;; + find all instances in buffer/project/directory and list them
;; + allow differrent fonts for different `comment-tags/keywords'
;;
;;; Code:


(require 'cl-lib)


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

(defcustom comment-tags/comment-start-only nil
  "Only highlight and track tags that are the beginning of a comment."
  :group 'comment-tags
  :type 'boolean)

(defcustom font-lock-comment-tags-face 'font-lock-comment-tags-face
  "Face for highlighting tags."
  :group 'comment-tags)

(defcustom comment-tags/foreground-color "Red"
  "Font foreground color."
  :group 'comment-tags
  :type 'string)
(defcustom comment-tags/background-color nil
  "Font background color."
  :group 'comment-tags
  :type 'string)

(defcustom comment-tags/keymap-prefix (kbd "C-c c")
  "Prefix for keymap."
  :group 'comment-tags
  :type 'string)

(defconst comment-tags/temp-buffer-name "*comment-tags*"
  "Name for temp buffers to list tags.")

(make-face 'font-lock-comment-tags-face)
(modify-face 'font-lock-comment-tags-face
             comment-tags/foreground-color
             comment-tags/background-color
             nil t nil t nil nil)
;; (defface comment-tags/face
;;   '((t :foreground "red" ;;'comment-tags/foreground-color
;;        :background nil ;;'comment-tags/background-color
;;        :weight bold
;;        :underline nil))
;;   "Font face for highlighted tags."
;;   :group 'comment-tags)

;;; funcs
(defun comment-tags--join (list joiner)
  "Helper function to join LIST of string with JOINER."
  (mapconcat 'identity list joiner))

(defun comment-tags/make-regexp ()
  "Create regexp from `comment-tags/keywords'."
  (concat "\\<\\(\\(?:" (comment-tags--join comment-tags/keywords "\\|") "\\):"
          (if (not comment-tags/require-colon)
              "?"
            "") "\\)"))

(defun comment-tags/syntax-propertize-function (start end)
  "Find tags in buffer between START and END.
Mark with `comment-tags/highlight' prop."
  (let ((case-fold-search nil)
        (inhibit-modification-hooks t))
    (goto-char start)
    (setq-local comment-tags/buffer-tags '())
    (remove-text-properties start end '(comment-tags/highlight))
    (funcall
     (syntax-propertize-rules
      ((comment-tags/make-regexp)
       (0 (ignore (comment-tags/find-tags)))))
     start end)))

(defun comment-tags/find-tags ()
  "Highlight tags in var `comment-tags/keywords'."
  (save-excursion
    (let* ((end-p (point))
           (start-p (match-beginning 0))
           (in-comment (and (nth 4 (syntax-ppss start-p)) (nth 4 (syntax-ppss end-p))))
           (comment-start (nth 8 (syntax-ppss start-p))))
      (when (and in-comment
                 (or (not comment-tags/comment-start-only)
                     (save-match-data
                       (string-match
                      (rx bol (0+ (not alphanumeric)) eol)
                      (buffer-substring-no-properties comment-start start-p)))))
        (put-text-property start-p end-p 'comment-tags/highlight (match-data))
        ;; push to local var for easy display
        (push (list (buffer-substring (line-beginning-position) (line-end-position))
                ;;TODO: add line num
               (match-data))
              comment-tags/buffer-tags)))))

(defun comment-tags/highlight-tags (limit)
  "Find areas marked with `comment-tags/highlight' and apply proper face within LIMIT."
  (let* ((pos (point))
         (chg (next-single-property-change pos 'comment-tags/highlight nil limit))
         (reg (comment-tags/make-regexp)))
    (when (and chg (> chg pos))
      (goto-char chg)
      (let ((value (get-text-property chg 'comment-tags/highlight)))
        (if value
            (progn
              (set-match-data value)
              t)
          (comment-tags/highlight-tags limit))))))

;;;###autoload
(defun comment-tags/list-tags-buffer ()
  ;;TODO: finish this
  "List all tags in the current buffer."
  (interactive)
  ;; (with-output-to-temp-buffer comment-tags/temp-buffer-name
  ;;   (with-current-buffer comment-tags/temp-buffer-name
  ;;     ;;(switch-to-buffer-other-window comment-tags/temp-buffer-name)
  ;;     (dolist (element comment-tags/buffer-tags)
  ;;       ;;(princ (format "%s" (car element)))
  ;;       (insert (format "%s" (car element)))
  ;;       )))
  (with-temp-buffer
    (dolist (element comment-tags/buffer-tags)
      (insert (format "%s" (car element)))
      ;;(insert (format "%s" (car element)))
      ))
    ;;(read comment-tags/buffer-tags))
  )

;; ;;;###autoload
;; (defun comment-tags/list-tags-project ()
;;   ;; TODO: finish this
;;   "List all tags in the current vcs project."
;;   (interactive)
;;   (with-output-to-temp-buffer comment-tags/temp-buffer-name
;;     (ag-regexp (comment-tags/make-regexp)
;;                (ag/project-root default-directory))))

;; ;;;###autoload
;; (defun comment-tags/list-tags-dir ()
;;   ;; TODO: finish this
;;   "List all tags in the current dir."
;;   (interactive)
;;   ;;()
;;   (message "comment-tags/list-tags-dir"))

;; TODO: search comments after tags?
;;;###autoload
(defun comment-tags-find-tags-buffer (&optional args)
  ;; TODO: finish this
  "List tags with ARGS in the current buffer."
  (interactive)
  (message "comment-tags/find-tags-buffer"))

;;
(defun comment-tags/enable ()
  "Enable comment-tags-mode."
  (set (make-local-variable 'syntax-propertize-function)
       #'comment-tags/syntax-propertize-function)
  (set (make-local-variable 'comment-tags/buffer-tags) '())
  (font-lock-add-keywords nil comment-tags/font-lock-keywords))

(defun comment-tags/disable ()
  "Disable comment-tags-mode."
  (set (make-local-variable 'syntax-propertize-function) nil)
  (font-lock-remove-keywords nil comment-tags/font-lock-keywords))


;;; vars
(defvar comment-tags/font-lock-keywords
  `((comment-tags/highlight-tags 1 font-lock-comment-tags-face t)))

(defvar-local comment-tags/buffer-tags '()
  "List of tags accumulated in current buffer.")

(defvar comment-tags/command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") #'comment-tags/list-tags-buffer)
    (define-key map (kbd "a") #'comment-tags/list-tags-buffers)
    map)
  "Command map.")
(fset 'comment-tags/command-map comment-tags/command-map)

(defvar comment-tags/mode-map
  (let ((map (make-sparse-keymap)))
    (message "map %s" comment-tags/command-map)
    (define-key map comment-tags/keymap-prefix 'comment-tags/command-map)
    map)
  "Keymap for Comment-Tags mode.")


;;;###autoload
(define-minor-mode comment-tags-mode
  "Highlight and navigate comment tags."
  :lighter "Comment Tags"
  :group 'comment-tags
  :require 'comment-tags
  :global nil
  :keymap comment-tags/mode-map
  (if comment-tags-mode
      (comment-tags/enable)
    (comment-tags/disable))
  (font-lock-mode 1))

(provide 'comment-tags)

;;; comment-tags.el ends here
