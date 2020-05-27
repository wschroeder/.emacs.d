(require 'ffap)

(defun org-completion-symbols ()
  (when (looking-back "=[a-zA-Z]+" nil)
    (let (cands)
      (save-match-data
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "=\\([a-zA-Z]+\\)=" nil t)
            (cl-pushnew
             (match-string-no-properties 0) cands :test 'equal))
          cands))
      (when cands
        (list (match-beginning 0) (match-end 0) cands)))))

(defun org-cap-filesystem ()
  (let (path)
    (when (setq path (ffap-string-at-point))
      (when (string-match "\\`file:\\(.*\\)\\'" path)
        (setq path (match-string 1 path)))
      (let ((compl (all-completions path #'read-file-name-internal)))
        (when compl
          (let* ((str (car compl))
                 (offset
                  (let ((i 0)
                        (len (length str)))
                    (while (and (< i len)
                                (equal (get-text-property i 'face str)
                                       'completions-common-part))
                      (cl-incf i))
                    i)))
            (list (- (point) offset) (point) compl)))))))

(setq completion-at-point-functions
      '(org-completion-symbols
        org-cap-filesystem))

