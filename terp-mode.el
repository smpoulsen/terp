;;; terp-mode.el -- A major mode for editing terp

;; Version: 0.0.1
;; Author: Sylvie Poulsen
;; Url: https://github.com/smpoulsen/terp
;; Keywords: languages, terp
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

(eval-when-compile (require 'rx)
                   (require 'racket-mode))

(defconst terp-mode-syntax-table
  (-let [table
         (copy-syntax-table racket-mode-syntax-table)]
    ;; syntax modifications
    table)
  "terp mode syntax table.")

;; Syntax highlighting for typeclasses and types
(defconst type-or-class-case
  (rx symbol-start
      (group upper (0+ (any word nonascii digit "_")))
      symbol-end))

(setq terp-functions
      '("car" "cdr" "compose" "cons"
        "empty?"
        "filter" "foldl" "foldr"
        "if"
        "length"
        "match" "map"
        "reverse"
        "sum"))
(setq terp-constants
      '("#t" "#f"))
(setq terp-keywords
      '("class"
        "data" "defn" "defrec"
        "instance"
        "lambda" "let" "letrec"
        "provide"
        "require"
        "type"))
(setq terp-operators
      '("->" "=>" ">" "<"))

;; generate regex string for each category of keywords
(setq terp-functions-regexp (regexp-opt terp-functions 'words))
(setq terp-constants-regexp (regexp-opt terp-constants 'words))
(setq terp-keywords-regexp (regexp-opt terp-keywords 'words))
(setq terp-operators-regexp (regexp-opt terp-operators 'symbols))

;; create the list for font-lock.
(setq terp-font-lock-keywords
      `(
        (,type-or-class-case 1 font-lock-type-face)
        (,terp-constants-regexp . font-lock-constant-face)
        (,terp-functions-regexp . font-lock-function-name-face)
        (,terp-keywords-regexp . font-lock-keyword-face)
        (,terp-operators-regexp . font-lock-builtin-face)
        ))

;;;###autoload
(define-derived-mode terp-mode racket-mode "terp"
  "Major mode for editing terp."

  ;; code for syntax highlighting
  (setq font-lock-defaults '((terp-font-lock-keywords))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tp?\\'" . terp-mode))

;; clear memory. no longer needed
(setq terp-constants nil)
(setq terp-keywords nil)
(setq terp-operators nil)
(setq terp-functions nil)

;; clear memory. no longer needed
(setq terp-constants-regexp nil)
(setq terp-keywords-regexp nil)
(setq terp-operators-regexp nil)
(setq terp-functions-regexp nil)

;; add the mode to the `features' list
(provide 'terp-mode)

;;; terp-mode.el ends here
