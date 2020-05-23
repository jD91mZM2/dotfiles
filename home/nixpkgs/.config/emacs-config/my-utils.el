(defun my/util/replace-region (beg end text)
  "Replace a region of text in the current buffer"
  (delete-region beg end)
  (save-excursion
    (goto-char beg)
    (insert text)))

(defun my/util/invert-case (string)
  "Return string but with the casing inverted. \"Hello\" => \"hELLO\""
  (map 'string
       (lambda (c)
         (if (or (<= ?a c ?z) (<= ?A c ?Z))
             (logxor c 32)
           c))
       string))

(defun my/util/current-directory ()
  "Get the filepath to the current directory"
  (file-name-directory (or
                        ;; When loading the file
                        load-file-name
                        ;; When evaluating the file
                        buffer-file-name)))

(defun my/util/relative (path)
  "Convert a relative path to an absolute one"
  (expand-file-name path (my/util/current-directory)))

(defvar my/util/font-lock-additions (make-hash-table))
(defun my/util/font-lock-extend (mode keywords)
  (let ((prev (gethash mode my/util/font-lock-additions)))
    (when prev
      (font-lock-remove-keywords mode prev))
    (font-lock-add-keywords mode keywords)
    (puthash mode keywords my/util/font-lock-additions)))

(provide 'my-utils)
