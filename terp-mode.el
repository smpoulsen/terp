;;; terp-mode.el -- A major mode for editing terp

;; Version: 0.0.1
;; Author: Travis Poulsen
;; Url: https://github.com/tpoulsen/terp
;; Keywords: languages, terp
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

(setq terp-functions
      '("if" "car" "cdr" "cons" "empty?"
        "foldl" "foldr" "reverse" "map"
        "sum" "length" "filter" "compose"))
(setq terp-type-defs
      '("class" "type" "instance"))
(setq terp-constants
      '("#t" "#f"))
(setq terp-keywords
      '("defn" "defrec" "let" "letrec" "lambda" "provide" "require"))
(setq terp-types
      '("Int" "String" "Bool" "->" "Float"))

;; generate regex string for each category of keywords
(setq terp-functions-regexp (regexp-opt terp-functions 'words))
(setq terp-type-defs-regexp (regexp-opt terp-type-defs 'words))
(setq terp-constants-regexp (regexp-opt terp-constants 'words))
(setq terp-keywords-regexp (regexp-opt terp-keywords 'words))
(setq terp-types-regexp (regexp-opt terp-types 'words))

;; create the list for font-lock.
(setq terp-font-lock-keywords
      `(
        (,terp-types-regexp . font-lock-type-face)
        (,terp-type-defs-regexp . font-lock-constant-face)
        (,terp-constants-regexp . font-lock-constant-face)
        (,terp-functions-regexp . font-lock-function-name-face)
        (,terp-keywords-regexp . font-lock-keyword-face)
        ))

;;;###autoload
(define-derived-mode terp-mode racket-mode "terp"
  "Major mode for editing terp."

  ;; code for syntax highlighting
  (setq font-lock-defaults '((terp-font-lock-keywords))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tp?\\'" . terp-mode))

;; clear memory. no longer needed
(setq terp-types nil)
(setq terp-type-defs nil)
(setq terp-keywords nil)
(setq terp-constants nil)
(setq terp-functions nil)

;; clear memory. no longer needed
(setq terp-types-regexp nil)
(setq terp-type-defs-regexp nil)
(setq terp-keywords-regexp nil)
(setq terp-constants-regexp nil)
(setq terp-functions-regexp nil)

;; add the mode to the `features' list
(provide 'terp-mode)

;;; terp-mode.el ends here
