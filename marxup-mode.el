(defvar marxup-mode-hook nil)

;; syntax table for comments, same as for haskell-mode
(defvar marxup-syntax-table
  (let ((st (make-syntax-table)))
       (modify-syntax-entry ?\{  "(" st)
       (modify-syntax-entry ?\}  ")" st)
       (modify-syntax-entry ?«   "(" st)
       (modify-syntax-entry ?»   ")" st)
       st))


(setq marxup-marxup-regexp "@\\([[:word:]_]+<-\\)?[[:word:]_]+")
(setq marxup-operators-regexp (regexp-opt '("«" "»")))
(setq marxup-font-lock-keywords
      `((,marxup-marxup-regexp . font-lock-function-name-face)
        (,marxup-operators-regexp . font-lock-keyword-face)))

(define-derived-mode marxup-mode text-mode
  "marxup mode"
  "Major mode for editing Haskell MarXup"
  
  :syntax-table marxup-syntax-table

  (setq font-lock-defaults '(marxup-font-lock-keywords))
  (setq mode-name "marxup"))

(provide 'marxup-mode)
