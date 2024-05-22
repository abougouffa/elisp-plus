;;; elisp-plus.el --- Better Emacs Lisp code viewing  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Abdelhak Bougouffa
;; SPDX-License-Identifier: MIT

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Keywords: lisp

;;; Commentary:

;; Adapted from Doom Emacs customization, with some modifications and additions.
;; This package adds some font-lock rules for Emacs Lisp code and overrides the
;; `calculate-lisp-indent' function with a better implementation if
;; `elisp-plus-better-lisp-indent' is non-nil (default to t).

;;; Code:

(require 'elisp-mode)
(autoload 'ad-get-orig-definition "advice")
(autoload 'cl-letf "cl-macs")


(defgroup elisp-plus nil
  "Better Emacs Lisp code viewing.")

(defcustom elisp-plus-better-lisp-indent t
  "Override `calculate-lisp-indent' for better indentation of quoted lists."
  :group 'elisp-plus
  :type 'boolean)

(defvar elisp-plus-face nil)
(defvar elisp-plus-calculate-lisp-indent-check-for-keyword nil)

;; Extracted from:
;; github.com/doomemacs/doomemacs/blob/master/modules/lang/emacs-lisp/autoload.el
(defun elisp-plus-highlight-vars-and-faces (end)
  "Match defined variables and functions.
Functions are differentiated into \"special forms\", \"built-in functions\" and
\"library/userland functions\"."
  (catch 'matcher
    (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
      (let ((ppss (save-excursion (syntax-ppss))))
        (cond ((nth 3 ppss)  ; strings
               (search-forward "\"" end t))
              ((nth 4 ppss)  ; comments
               (forward-line +1))
              ((let ((symbol (intern-soft (match-string-no-properties 0))))
                 (and (cond ((null symbol) nil)
                            ((eq symbol t) nil)
                            ((keywordp symbol) nil)
                            ((special-variable-p symbol)
                             (setq elisp-plus-face 'font-lock-variable-name-face))
                            ((and (fboundp symbol)
                                  (eq (char-before (match-beginning 0)) ?\()
                                  (not (memq (char-before (1- (match-beginning 0)))
                                             (list ?\' ?\`))))
                             (let ((unaliased (indirect-function symbol)))
                               (unless (or (macrop unaliased)
                                           (special-form-p unaliased))
                                 (let (unadvised)
                                   (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                                   (setq unaliased (indirect-function unadvised)))))
                                   unaliased)
                                 (setq elisp-plus-face
                                       (if (subrp unaliased)
                                           'font-lock-constant-face
                                         'font-lock-function-name-face))))))
                      (throw 'matcher t)))))))
    nil))

;; Taken from:
;; reddit.com/r/emacs/comments/d7x7x8/finally_fixing_indentation_of_quoted_lists
(defun elisp-plus-calculate-lisp-indent (&optional parse-start)
  "Add better indentation for quoted and backquoted lists."
  ;; The `calculate-lisp-indent-last-sexp' is defined with `defvar' with it's
  ;; value omitted, marking it special and only defining it locally. So if you
  ;; don't have this, you'll get a void variable error.
  (defvar calculate-lisp-indent-last-sexp)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          ;; Setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          state calculate-lisp-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start) (beginning-of-defun))
            (t (setq state parse-start)))
      (unless state
        ;; Find outermost containing sexp
        (while (< (point) indent-point)
          (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry state (> (elt state 0) 0))
        (setq retry nil
              containing-sexp (elt state 1)
              calculate-lisp-indent-last-sexp (elt state 2))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (unless retry
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
            ;; `indent-point' immediately follows open paren. Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s("))
                ;; First element of containing sexp is a list. Indent under that
                ;; list.
                ((> (save-excursion (forward-line 1) (point)) calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (or
                      ;; Containing sexp has nothing before this line except the
                      ;; first element. Indent under that element.
                      (= (point) calculate-lisp-indent-last-sexp)

                      ;; First sexp after `containing-sexp' is a keyword. This
                      ;; condition is more debatable. It's so that I can have
                      ;; unquoted plists in macros. It assumes that you won't
                      ;; make a function whose name is a keyword.
                      (and elisp-plus-calculate-lisp-indent-check-for-keyword
                           (when-let (char-after (char-after (1+ containing-sexp)))
                             (char-equal char-after ?:)))

                      ;; Check for quotes or backquotes around.
                      (let* ((positions (elt state 9))
                             (last (car (last positions)))
                             (rest (reverse (butlast positions)))
                             (any-quoted-p nil)
                             (point nil))
                        (or
                         (when-let (char (char-before last))
                           (or (char-equal char ?')
                               (char-equal char ?`)))
                         (progn
                           (while (and rest (not any-quoted-p))
                             (setq point (pop rest)
                                   any-quoted-p
                                   (or
                                    (when-let (char (char-before point))
                                      (or (char-equal char ?') (char-equal char ?`)))
                                    (save-excursion
                                      (goto-char (1+ point))
                                      (looking-at-p "\\(?:back\\)?quote[\t\n\f\s]+(")))))
                           any-quoted-p))))
                     ;; Containing sexp has nothing before this line except the
                     ;; first element. Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'. Again, it's almost
                 ;; certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by `lisp-indent-offset' or
      ;; if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case `calculate-lisp-indent-last-sexp' is not `nil'
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment or it does not apply
                ;; to this argument, try to align a constant-symbol under the
                ;; last preceding constant symbol, if there is such one of the
                ;; last 2 preceding symbols, in the previous uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation where it
                     ;; begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace following an
                       ;; open paren. (Bug#1012)
                       (backward-prefix-chars)
                       (while (not (or (looking-back "^[ \t]*\\|([ \t]+" (line-beginning-position))
                                       (and containing-sexp (>= (1+ containing-sexp) (point)))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position) (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant as
                ;; defined above.
                normal-indent))
              ;; in this case `calculate-lisp-indent-last-sexp' is `nil'
              (desired-indent)
              (t
               normal-indent))))))

;;;###autoload
(define-minor-mode elisp-plus-mode
  "A global minor mode for better Emacs Lisp code."
  :global t
  (if elisp-plus-mode
      (progn
        (font-lock-add-keywords 'emacs-lisp-mode '((elisp-plus-highlight-vars-and-faces . elisp-plus-face)))
        (when elisp-plus-better-lisp-indent
          (advice-add 'calculate-lisp-indent :override #'elisp-plus-calculate-lisp-indent)))
    (advice-remove 'calculate-lisp-indent #'elisp-plus-calculate-lisp-indent)
    (font-lock-remove-keywords 'emacs-lisp-mode '((elisp-plus-highlight-vars-and-faces . elisp-plus-face)))))


(provide 'elisp-plus)
;;; elisp-plus.el ends here
