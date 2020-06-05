(require 'yaml-mode)

;;; ------------------------------------------------------------
;;; Additional commands for YAML
;;; ------------------------------------------------------------

(defmacro yaml-mode-until (condition &rest body)
  "Defines a repeat..until style loop wherein the body is
executed at least once and the condition is checked after the
first iteration for future iterations.

Example:

  (yaml-mode-until (condition-met)
    (statement1)
    (statement2))"
  `(progn ,@body
          (while ,condition ,@body)))

(defun yaml-mode-kill-entry ()
  "In YAML mode, kill the entry at point, which includes everything in its hierarchy."
  (interactive)
  (back-to-indentation)
  (save-mark-and-excursion
    (let ((column (current-column))
          (times  (abs (or current-prefix-arg 1)))
          (max-point (buffer-size)))
      (dotimes (_ times)
        (yaml-mode-until (and (< (point) max-point)
                              (< column (current-column)))
          (move-beginning-of-line nil)
          (kill-whole-line)
          (back-to-indentation))))))

(defun yaml-mode-forward-entry-same-level ()
  "In YAML mode, move forward to the next entry with the same
hierarchical level. If there are no more entries in the current
hierarchy of the same level, this command means to move to the
next higher-hierarchical entry."
  (interactive)
  (back-to-indentation)
  (let ((direction (cl-signum (or current-prefix-arg 1)))
        (column    (current-column))
        (times     (abs (or current-prefix-arg 1)))
        (max-point (buffer-size)))
    (dotimes (_ times)
      (yaml-mode-until (and (< (point) max-point)
                            (> (point) 0)
                            (or (< column (current-column))
                                (not (symbol-at-point))))
         (forward-line direction)
         (back-to-indentation)))))

(defun yaml-mode-previous-entry-same-level ()
  "In YAML mode, move backward to the previous entry with the
same hierarchical level. If there are no preceding entries in the
current hierarchy of the same level, this command means to move
to the previous higher-hierarchical entry."
  (interactive)
  (let ((current-prefix-arg (- (or current-prefix-arg 1))))
    (call-interactively 'yaml-mode-forward-entry-same-level)))


;;; ------------------------------------------------------------
;;; Repeat wrappers
;;; ------------------------------------------------------------

(defun yaml-mode-repeat-command (command)
  "Repeat an interactive command."
  (require 'repeat)
  (let ((repeat-previous-repeated-command  command)
        (repeat-message-function           #'ignore)
        (last-repeatable-command           'repeat))
    (repeat nil)))

(defun yaml-mode-kill-entry-repeat ()
  "In YAML mode, kill the entry at point, which includes everything in its hierarchy.

You can repeat this by hitting the last key again."
  (interactive)
  (yaml-mode-repeat-command 'yaml-mode-kill-entry))

(defun yaml-mode-forward-entry-same-level-repeat ()
  "In YAML mode, move forward to the next entry with the same
hierarchical level. If there are no more entries in the current
hierarchy of the same level, this command means to move to the
next higher-hierarchical entry.

You can repeat this by hitting the last key again."
  (interactive)
  (yaml-mode-repeat-command 'yaml-mode-forward-entry-same-level))

(defun yaml-mode-previous-entry-same-level-repeat ()
  "In YAML mode, move backward to the previous entry with the
same hierarchical level. If there are no preceding entries in the
current hierarchy of the same level, this command means to move
to the previous higher-hierarchical entry.

You can repeat this by hitting the last key again."
  (interactive)
  (yaml-mode-repeat-command 'yaml-mode-previous-entry-same-level))


;;; ------------------------------------------------------------
;;; Keybindings
;;; ------------------------------------------------------------

(define-key yaml-mode-map (kbd "C-c C-k") 'yaml-mode-kill-entry-repeat)
(define-key yaml-mode-map (kbd "C-c C-f") 'yaml-mode-forward-entry-same-level-repeat)
(define-key yaml-mode-map (kbd "C-c C-b") 'yaml-mode-previous-entry-same-level-repeat)

