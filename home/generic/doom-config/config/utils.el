;;; utils.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun my/replace-region (beg end text)
  "Replace a region of text in the current buffer"
  (delete-region beg end)
  (save-excursion
    (goto-char beg)
    (insert text)))

(defun my/invert-case (string)
  "Return string but with the casing inverted. \"Hello\" => \"hELLO\""
  (cl-map 'string
          (lambda (c)
            (if (or (<= ?a c ?z) (<= ?A c ?Z))
                (logxor c 32)
              c))
          string))

(defun my/with-inverted-case (beg end inner)
  ;; Invert casing
  (my/replace-region beg end (my/invert-case (buffer-substring-no-properties beg end)))
  ;; Execute inner function
  (funcall inner)
  ;; Invert casing back
  (my/replace-region beg end (my/invert-case (buffer-substring-no-properties beg end))))

(defvar my/font-lock-additions (make-hash-table))
(defun my/font-lock-extend (mode keywords)
  (let ((prev (gethash mode my/font-lock-additions)))
    (when prev
      (font-lock-remove-keywords mode prev))
    (font-lock-add-keywords mode keywords)
    (puthash mode keywords my/font-lock-additions)))
