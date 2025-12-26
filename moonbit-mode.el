;;; moonbit-mode.el --- Simple MoonBit major mode (read-focused) -*- lexical-binding: t; -*-

;; Read-focused: syntax highlighting + comments/strings.
;; No external binaries, no tree-sitter required.

(require 'rx)

(defgroup moonbit nil
  "MoonBit language support."
  :group 'languages)

(defcustom moonbit-indent-offset 2
  "Indent offset for `moonbit-mode'."
  :type 'integer
  :group 'moonbit)

;; ---------- Syntax table ----------
(defvar moonbit-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Treat '_' as word constituent
    (modify-syntax-entry ?_ "w" st)

    ;; Line comment: //
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)

    ;; Block comment: /* ... */
    (modify-syntax-entry ?* ". 23" st)

    ;; Strings: "..."
    (modify-syntax-entry ?\" "\"" st)

    ;; Character-ish literal: 'a' (best-effort)
    (modify-syntax-entry ?' "\"" st)

    st)
  "Syntax table for `moonbit-mode'.")

;; ---------- Keywords ----------
(defconst moonbit--keywords
  '("let" "mut" "fn" "type" "struct" "enum" "match"
    "if" "else" "while" "for" "in"
    "return" "break" "continue"
    "pub" "import" "as"
    "trait" "impl" "where"
    "use" "module")
  "MoonBit keywords for basic highlighting.")

(defconst moonbit--builtins
  '("true" "false" "None" "Some" "Ok" "Err" "panic" "assert")
  "Common builtins / constructors (best-effort).")

;; Types: Capitalized identifiers are often types/constructors.
(defconst moonbit--rx-type
  (rx symbol-start (group (any "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
                               "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
                          (* (or word ?_))) symbol-end))

;; Functions: identifier followed by "("
(defconst moonbit--rx-func-call
  (rx symbol-start (group (+ (or word ?_))) (* space) "("))

;; Definition names:
;; - fn name(
;; - type Name
;; - struct Name
;; - enum Name
(defconst moonbit--rx-def-name
  (rx symbol-start
      (or "fn" "type" "struct" "enum" "trait" "module")
      (+ space)
      (group (+ (or word ?_))))
  "Best-effort regex for definition names.")

;; Numbers (int/float, allow underscores)
(defconst moonbit--rx-number
  (rx symbol-start
      (or
       ;; 0x..., 0b..., 0o...
       (seq "0" (any "xX") (+ (or (char "0-9") (char "a-f") (char "A-F") ?_)))
       (seq "0" (any "bB") (+ (or (char "0-1") ?_)))
       (seq "0" (any "oO") (+ (or (char "0-7") ?_)))
       ;; decimal / float
       (seq (+ (or (char "0-9") ?_))
            (opt (seq "." (+ (or (char "0-9") ?_))))
            (opt (seq (any "eE") (opt (any "+-")) (+ (or (char "0-9") ?_))))))
      symbol-end))

;; ---------- Font-lock ----------
(defvar moonbit-font-lock-keywords
  (let ((kw (regexp-opt moonbit--keywords 'symbols))
        (bi (regexp-opt moonbit--builtins 'symbols)))
    `(
      ;; Keywords
      (,kw . font-lock-keyword-face)

      ;; Builtins / constructors
      (,bi . font-lock-builtin-face)

      ;; Definition names
      (,moonbit--rx-def-name 1 font-lock-function-name-face)

      ;; Function calls (lighter)
      (,moonbit--rx-func-call 1 font-lock-function-name-face)

      ;; Types / Constructors
      (,moonbit--rx-type 1 font-lock-type-face)

      ;; Numbers
      (,moonbit--rx-number . font-lock-number-face)

      ;; Attributes / annotations: @foo
      (,(rx "@" (group (+ (or word ?_))))
       1 font-lock-preprocessor-face)

      ;; Module path-ish: Foo::Bar
      (,(rx (group (+ (or word ?_))) "::" (group (+ (or word ?_))))
       (1 font-lock-namespace-face)
       (2 font-lock-namespace-face))

      ;; Field-ish: .name
      (,(rx "." (group (+ (or word ?_))))
       1 font-lock-variable-name-face)
      ))
  "Basic font-lock keywords for `moonbit-mode'.")

;; ---------- Indentation (minimal / best-effort) ----------
(defun moonbit--line-starts-with-closing-p ()
  (save-excursion
    (back-to-indentation)
    (looking-at-p (rx (any "}" ")" "]")))))

(defun moonbit--count-unclosed-braces (pos)
  "Count '{' - '}' from beginning to POS, ignoring strings/comments best-effort."
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) pos)
        (cond
         ;; Skip strings/comments using syntax-ppss
         ((nth 8 (syntax-ppss))
          (goto-char (or (nth 8 (syntax-ppss)) (1+ (point)))))
         (t
          (let ((ch (char-after)))
            (cond
             ((eq ch ?{) (setq count (1+ count)))
             ((eq ch ?}) (setq count (1- count)))))
          (forward-char 1))))
      (max 0 count))))

(defun moonbit-indent-line ()
  "Very small indentation function for MoonBit."
  (interactive)
  (let* ((pos (point))
         (bol (line-beginning-position))
         (eol (line-end-position))
         (cur-indent (current-indentation))
         (base (moonbit--count-unclosed-braces bol))
         (indent (* base moonbit-indent-offset)))
    (when (moonbit--line-starts-with-closing-p)
      (setq indent (max 0 (- indent moonbit-indent-offset))))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to indent))
    (when (> (- pos bol) cur-indent)
      (goto-char (+ bol indent)))))

;; ---------- Mode ----------
;;;###autoload
(define-derived-mode moonbit-mode prog-mode "MoonBit"
  "Simple MoonBit major mode (Elisp-only, read-focused)."
  :syntax-table moonbit-mode-syntax-table
  (setq-local font-lock-defaults '(moonbit-font-lock-keywords))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx (or "//" "/*") (+ space)))
  (setq-local indent-line-function #'moonbit-indent-line)

  ;; Treat /* */ as comments for navigation
  (setq-local comment-multi-line t)

  ;; Basic electric pairs
  (setq-local electric-pair-pairs '((?\{ . ?\}) (?\( . ?\)) (?\[ . ?\]) (?\" . ?\")))
  (electric-pair-local-mode 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mbt\\'" . moonbit-mode))

(provide 'moonbit-mode)
;;; moonbit-mode.el ends here
