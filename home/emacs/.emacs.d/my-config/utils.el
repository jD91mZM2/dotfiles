(defun my/util/replace-region (beg end text)
  "Replace a region of text in the current buffer"
  (delete-region beg end)
  (save-excursion
    (goto-char beg)
    (insert text)))

(defun my/util/invert-case (string)
  "Return string but with the casing inverted. \"Hello\" => \"hELLO\""
  (map 'string (lambda (c) (logxor c 32)) string))
