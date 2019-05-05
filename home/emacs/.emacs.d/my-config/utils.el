(defun relative (path)
  "Convert a relative path to an absolute one"
  (expand-file-name path (file-name-directory load-file-name)))
