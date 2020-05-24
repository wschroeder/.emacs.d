(require 'auto-complete)

;; Autocomplete config
(define-key ac-completing-map [return] nil) ; no enter (1.)
(define-key ac-completing-map "\r" nil) ; no enter (2.)
(define-key ac-completing-map "\t" 'ac-complete) ; use tab to complete
