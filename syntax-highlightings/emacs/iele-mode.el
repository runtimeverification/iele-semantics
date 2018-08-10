;; iele-mode.el
;; Copyright - Runtime Verification, Inc.
;; Reference: https://www.emacswiki.org/emacs/ModeTutorial
;;            http://ergoemacs.org/emacs/elisp_syntax_coloring.html
;;            https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
;;            https://github.com/llvm-mirror/llvm/blob/master/utils/emacs/llvm-mode.el

;; Usage: add the code below to your .emacs file:
;;     (load "path/to/this/file" )
;;     (add-to-list 'auto-mode-alist '("\\.iele$" . iele-mode)) ;; to launch iele-mode for .iele files

;; syntax table for iele-mode
(defvar iele-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?% "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?. "_" table)
    ; (modify-syntax-entry ?\; ". " table)
    ; (modify-syntax-entry ?\n "> " table)
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table used while in IELE mode.")

;; create the list for font-lock.
(setq iele-font-lock-keywords
  (list
   ;; Variables
   ;; '("[%@][-a-zA-Z$\._][-a-zA-Z$\._0-9]*" . font-lock-variable-name-face)
   ;; Labels
   '("[-a-zA-Z$\._0-9]+:" . font-lock-variable-name-face)
   ;; Integer literals
   '("\\b[-]?[0-9]+\\b" . font-lock-constant-face)
   ;; Floating point constants
   '("\\b[-+]?[0-9]+\.[0-9]*\([eE][-+]?[0-9]+\)?\\b" . font-lock-constant-face)
   ;; Hex constants
   '("\\b0x[0-9A-Fa-f]+\\b" . font-lock-constant-face)
   ;; Void constant
   `(,(regexp-opt '("true" "false" "void") 'symbols) . font-lock-constant-face)
   ;; Contract declaration
   '("\\(contract\\)\s+\\([-a-zA-Z$\.0-9_]+\\|\".+?\"\\)" (1 font-lock-keyword-face) (2 font-lock-function-name-face))  
   ;; Function declaration
   '("\\(define\\)\s+\\(?:public\s+\\)?\\([%@]\\([-a-zA-Z$\.0-9_]+\\|\".+?\"\\)\\)" (1 font-lock-keyword-face) (2 font-lock-function-name-face))
   ;; Keywords
   `(,(regexp-opt '("ret" "revert" "br" "at" "load" "store" "sload" "sstore"
                    "log" "create" "copycreate" "selfdestruct" "deposit" "init" "send" "gaslimit"
                    "iszero" "not" "add" "sub" "mul" "div" "exp" "mod" "addmod" "mulmod" "expmod"
                    "byte" "sext" "twos"
                    "and" "or" "xor" "shift" "cmp" "lt" "le" "gt" "ge" "eq" "ne" "sha3"
                    "external" "contract" "define" "public"
                    "call" "staticcall") 'symbols) . font-lock-keyword-face)
   ))

(define-derived-mode iele-mode fundamental-mode "iele mode"
  "Major mode for editing IELE"

  (setq font-lock-keywords-only t) ;; turn off "..." double quotes

  ;; code for syntax highlighting
  (setq font-lock-defaults '((iele-font-lock-keywords))))

;; add the mode to the `features' list
(provide 'iele-mode)
