;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;;; Allow extra space at the end of the line
(setq-default fill-column 80)

;;; Example of setting a variable
;;; This particular example causes the current line number to be shown
;;; Remove the ; in front to turn this feature on.
; (setq-default line-number-mode t)

;;; Example of binding a key
;;; This particular example binds "ESC =" to the "goto-line" function.
;;; Remove the ; in front to turn this feature on.
; (global-set-key "\M-="  'goto-line)

(autoload 'matlab-mode "~/emacs/matlab.el" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(setq matlab-highlight-block-match-flag nil)
(setq matlab-indent-function-body t)  ; if you want function bodies indented
(setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
(defun my-matlab-mode-hook ()
  (setq auto-fill-function nil)
  (setq fill-column 80))
(setq matlab-mode-hook `my-matlab-mode-hook)

(autoload 'python-mode "~/emacs/python-mode.el" "Enter Python mode." t)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))
(defun my-python-mode-hook ()
  (set-face-foreground `py-builtins-face "red")
  (set-face-foreground `py-pseudo-keyword-face "red"))
(setq python-mode-hook `my-python-mode-hook)

(autoload 'js2-mode "~/emacs/js2-mode.elc" "Enter JavaScript mode." t)
(setq auto-mode-alist (cons '("\\.js\\'" . js2-mode) auto-mode-alist))

(autoload 'yaml-mode "~/emacs/yaml-mode.el" "Enter YAML mode." t)
(setq auto-mode-alist (cons '("\\.yaml\\'" . yaml-mode) auto-mode-alist))

(when (fboundp 'winner-mode) (winner-mode 1))

(global-set-key "\M-g" 'goto-line)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq scroll-step 2)

;(setq-default auto-fill-function 'do-auto-fill)
(setq-default auto-fill-function nil)
(setq-default text-mode-hook 'turn-on-auto-fill)
(setq-default c-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 80)
(setq-default line-number-mode t)
(setq-default column-number-mode t)
(global-font-lock-mode 1)

(add-hook 'nxml-mode-hook  (lambda () (message "XML")))
(add-hook 'sgml-mode-hook 'turn-off-auto-fill) ; <- this is the one that works for me

; (list-colors-display)
; White background for when the lights are on.
(defun bg-white ()
  (interactive)
  (set-face-foreground `bold "black")
  (set-face-foreground `font-lock-function-name-face "blue")
  (set-face-foreground `font-lock-comment-face "firebrick4")
  (set-face-foreground `font-lock-string-face "purple")
  (set-face-foreground `font-lock-type-face "red")
  (set-face-foreground `font-lock-keyword-face "blue")
  (set-face-foreground `font-lock-builtin-face "brown")
  (set-face-foreground `font-lock-constant-face "dark green")
  (set-face-foreground `font-lock-negation-char-face "red")
  (set-face-foreground `font-lock-variable-name-face "dark green")
  ;(set-background-color "gray88") ;wb
  (set-background-color "gray70")
  (set-face-foreground `default "black"))
; Black background, my favorite.
(defun bg-black ()
  (interactive)
  (set-face-foreground `bold "lemonchiffon1")
  (set-face-foreground `font-lock-function-name-face "deep sky blue")
  (set-face-foreground `font-lock-comment-face "gray66") ;light sea green") ;"indian red")
  (set-face-foreground `font-lock-string-face "gray49") ; "DarkOrange3" "IndianRed1"
  (set-face-foreground `font-lock-type-face "red")
  (set-face-foreground `font-lock-keyword-face "forest green")
  (set-face-foreground `font-lock-builtin-face "deep sky blue")
  (set-face-foreground `font-lock-constant-face "DodgerBlue1")
  (set-face-foreground `font-lock-variable-name-face "deep sky blue")
  (set-face-foreground `font-lock-negation-char-face "green")
  (set-background-color "black") ;bb
  (set-face-foreground `default "white"))
(bg-black)

;(set-face-attribute 'comint-highlight-prompt nil :inherit nil)
;(set-face-foreground 'minibuffer-prompt "green")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default c-default-style "gnu") ;"linux")

;; macros
; 1) C-x (, type, C-x )
; 2) name-last-kbd-macro
; 3) insert-kbd-macro
;; Insert a line, or a region of text, at the end of the other frame, and put
;; the pointer there. This is useful for shells like matlab, python, sage.
(fset 'shyank
      [?\C-x ?o escape ?> ?\C-y])
(fset 'shyankl
      [?\C-a ?\C-  ?\C-e ?\M-w ?\C-x ?o ?\C-y])
;(global-set-key [f6] 'shyankl)
;(global-set-key [f6] 'shyank)
(fset 'previous-shell-command-line "\C-rl@\C-x")
(global-set-key [f11] 'previous-shell-command-line)

(fset 'toend [escape ?>]) (global-set-key [f5] 'toend)
(fset 'cdup "../") (global-set-key [f7] 'cdup)

;(global-set-key (kbd "M-[ d") `backward-word)
;(global-set-key (kbd "M-[ c") `forward-word)
;(global-set-key (kbd "M-[ A") `backward-paragraph)
;(global-set-key (kbd "M-[ B") `forward-paragraph)
(global-set-key (kbd "M-[") `backward-paragraph)
(global-set-key (kbd "M-]") `forward-paragraph)

(global-set-key (kbd "C-x p") 'previous-multiframe-window)
(global-set-key (kbd "C-x i") 'next-multiframe-window)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key [f10] 'find-alternate-file)
(global-set-key [f12] 'buffer-menu)
;(global-set-key (kbd "C-x b") 'switch-to-buffer) ; default
(global-set-key (kbd "C-n") 'rename-buffer)
(global-set-key (kbd "C-q") 'query-replace)
(global-set-key (kbd "C-`") 'compilation-shell-minor-mode)
(global-set-key (kbd "C-f") 'auto-fill-mode)

(defun vu () (interactive) (shell-command "amixer -q sset Master 3%+"))
(defun vd () (interactive) (shell-command "amixer -q sset Master 3%-"))

;; Argh, which one works?
;; (c-add-style "my-cc-style" '(c-offsets-alist . ((innamespace . 0))))
;; (defun my-c++-mode-hook ()
;;   (c-set-style "my-cc-style"))
;; (add-hook 'c++-mode-common-hook
;;           '(lambda()
;;              (c-set-style "my-cc-style")))

;(add-to-list 'c++-mode-common-hook (lambda () (c-set-offset 'innamespace 0)))

;; This is useful for responding to compiler errors.
;;later Ha! See below.
;;todo I'd like this to find the number at the current point and use that rather
;; than use car of the kill-ring.
;;todo Even cooler would be to parse a string like this:
;;     src/Mesh.hpp:151
;; In detail, search the current line using regexp for all such strings. If just
;; one, go to next step; if more than one, query the user. Next, switch the
;; other frame to that line number and go to that line number.
(defun goto-line-next-multiframe-window ()
  "goto-line in the other frame window corresponding to the
number string that is the car of the kill-ring."
  (interactive)
  (let ((lineno (string-to-number (car kill-ring-yank-pointer))))
    (cond ((not (= lineno 0)) (progn (next-multiframe-window)
                                     (goto-line lineno)))
          (t (message "Invalid line number.")))))
(global-set-key [f4] 'goto-line-next-multiframe-window)

(put 'narrow-to-page 'disabled nil)

(defun format-cpp ()
  "Put a space between ) and { in a function definition. I like
that format because it makes searching for the defintion, as
opposed to the function's use, easy."
  (interactive)
  (replace-regexp
   "\\([a-zA-Z_0-9]+\\)\\((\\([^)]* [^)]*\\|\\))[\n| ]\\(const\\)[\n| ]{\\)"
   "\\1 \\2"))

;; ctrl-x 4 c to clone current buffer into other frame

;; so i can write:
;;   void foo (
;;     int arg1, ...)
;; note this indentation is activated in both decls and calls only if ( is the
;; EOL; otherwise, default is the usual alignment.
(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-indent-setup)
(add-hook 'c-mode-hook 'my-indent-setup)

;; I don't want to set this because text-mode is probably what I want most of
;; the time.
;(setq auto-mode-alist (cons '("\\.txt\\'" . paragraph-indent-text-mode) auto-mode-alist))

;; completion window override
(defun comint-close-completions ()
  "Close the comint completions buffer.
Used in advice to various comint functions to automatically close
the completions buffer as soon as I'm done with it. Based on
Dmitriy Igrishin's patched version of comint.el."
  (if comint-dynamic-list-completions-config
      (progn
        (set-window-configuration comint-dynamic-list-completions-config)
        (setq comint-dynamic-list-completions-config nil))))
(defadvice comint-send-input (after close-completions activate)
  (comint-close-completions))
(defadvice comint-dynamic-complete-as-filename (after close-completions activate)
  (if ad-return-value (comint-close-completions)))
(defadvice comint-dynamic-simple-complete (after close-completions activate)
  (if (member ad-return-value '('sole 'shortest 'partial))
      (comint-close-completions)))
(defadvice comint-dynamic-list-completions (after close-completions activate)
  (comint-close-completions)
  (if (not unread-command-events)
      ;; comint's "Type space to flush" swallows space. put it back in.
      (setq unread-command-events (listify-key-sequence " "))))

;; this is compilation magic. however, it messes with matlab and probably other
;; command lines run in a shell.
;(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(when (fboundp 'winner-mode) (winner-mode 1))

(add-to-list 'load-path "~ambradl/emacs/")
(require 'lcomp)
(lcomp-mode 1)

;;; fonts
;(set-default-font "-misc-fixed-medium-r-semicondensed-*-13-120-75-75-c-60-koi8-*")
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))
;; Useful:
;(print (font-family-list))
;(set-frame-font "DejaVu Sans Mono-10" t)

(setq default-frame-alist
      '((top . 0) (left . 99)
        (width . 205) (height . 100)))

(setq dired-listing-switches "-lRGh --group-directories-first")

(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)

;; to view kill-ring buffer: C-h v kill-ring

;;; From gregb on stackoverflow. Not sure why it works only on the right
;;; frame. Once I've studied ELisp libs a bit more, fix this.
(defun my-shift-window-right (&optional start-window)
  "Reset the current window configuration with START-WINDOW
on the right and the rest of the windows on the left. START-WINDOW defaults to
the selected window. Return START-WINDOW, or nil if START-WINDOW isn't live or
if there is only one visible window."
  (interactive (list (selected-window)))
  (if (or (one-window-p)
          (and start-window
               (not (window-live-p start-window)))) nil
    (let ((other-buffers '())
          (start-window (or start-window (selected-window))))
      ;; add all visible buffers other than the current one to other-buffers list
      (walk-windows #'(lambda (window)
                        (when (not (eq window start-window))
                          (add-to-list 'other-buffers (window-buffer window)))))
      (delete-other-windows)
      ;; pop the first "other buffer" into a split window on the left
      (set-window-buffer (select-window (split-window start-window nil 'left))
                         (pop other-buffers))
      ;; make a split window for each buffer in the "other-buffers" list
      ;; select the start-window and return it when finished
      (dolist (buffer other-buffers (select-window start-window))
        (set-window-buffer (split-window (selected-window) nil 'above) buffer)))))

; Add cmake listfile names to the mode list.
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))
(autoload 'cmake-mode "~/emacs/cmake-mode.el" t)

(setq auto-mode-alist
      (append
       '(("\\.f90\\'" . f90-mode))
       '(("\\.F90\\'" . f90-mode))
       '(("\\.F\\'" . f90-mode))
       auto-mode-alist))

(fset 'amb-include "#include \"/home/ambradl/climate/sik/hommexx/dbg.hpp\"")
(fset 'amb-omp "#if (defined COLUMN_OPENMP && !defined __bg__)\n!$omp\n#endif")

(defun amb-shells (buf1-name buf1-command buf2-name buf2-command)
  (save-current-buffer
    (set-buffer buf1-name)
    (insert (concat buf1-command "; ~/bin/amb-signal.sh"))
    (comint-send-input)
    (set-buffer buf2-name)
    (insert (concat "~/bin/amb-wait.sh; " buf2-command))
    (comint-send-input)))

(defun mysr ()
  (replace-regexp
   "\\([a-zA-Z_0-9]+\\)\\((\\([^)]* [^)]*\\|\\))[\n| ]\\(const\\)[\n| ]{\\)"
   "\\1 \\2"))

; So a 'word' can be separated by underscores.
(modify-syntax-entry ?_ "_")

(defun if0 ()
  (interactive)
  (insert "if [ $? == 0 ]; then "))

;;; emacs can take a bit to open a version-controlled file. i don't use this
;;; feature, so turn it off.
(setq vc-handled-backends nil)

;;; use xev to find out keycodes
(global-set-key (kbd "<Scroll_Lock>") ctl-x-map)
(global-set-key (kbd "<Super_R>") ctl-x-map)


;;; A pseudocode mode mod'ed from ergoemacs.org's tutorial on syntax
;;; highlighting. ! is a comment, keywords are foreach, solve, suchthat.
(setq pseudocode-highlights
      '(("foreach\\|suchthat" . font-lock-keyword-face)
        (":=\\|=\\+=\\|+\\|/=\\|/" . font-lock-type-face)))
(defvar pseudocode-mode-syntax-table nil "Syntax table for `pseudocode-mode'.")
(setq pseudocode-mode-syntax-table
      (let ( (synTable (make-syntax-table)))
        ;; set/modify each char's class
        (modify-syntax-entry ?! "<" synTable)
        (modify-syntax-entry ?\n ">" synTable)
        ;; more lines here ...

        ;; return it
        synTable))
(define-derived-mode pseudocode-mode fundamental-mode "pseudocode"
  "major mode for editing pseudocode."
  (setq font-lock-defaults '(pseudocode-highlights))
  ;; then, have this line inside your mode definition. So that, when user calls
  ;; your major mode, it will set syntax table for whatever is the current
  ;; buffer of user
  (set-syntax-table pseudocode-mode-syntax-table))

(load "amb.el")

(defun nice-paren ()
  (keyboard-translate (get-byte 0 "(") (get-byte 0 "{"))
  (keyboard-translate (get-byte 0 ")") (get-byte 0 "}"))
  (keyboard-translate (get-byte 0 "[") (get-byte 0 "("))
  (keyboard-translate (get-byte 0 "]") (get-byte 0 ")"))
  (keyboard-translate (get-byte 0 "{") (get-byte 0 "["))
  (keyboard-translate (get-byte 0 "}") (get-byte 0 "]")))
(defun undo-paren ()
  (keyboard-translate (get-byte 0 "(") (get-byte 0 "("))
  (keyboard-translate (get-byte 0 ")") (get-byte 0 ")"))
  (keyboard-translate (get-byte 0 "[") (get-byte 0 "["))
  (keyboard-translate (get-byte 0 "]") (get-byte 0 "]"))
  (keyboard-translate (get-byte 0 "{") (get-byte 0 "{"))
  (keyboard-translate (get-byte 0 "}") (get-byte 0 "}")))
(nice-paren)
;;(undo-paren)

(require 'hy-mode)
(setq hy-mode-inferior-lisp-command "~/.local/bin/hy")

(require 'autopair)
(require 'paredit)
(require 'rainbow-delimiters)
(dolist (mh '(hy scheme lisp emacs-lisp inferior-lisp f90 c++))
  (let ((hook-name (intern (concat (symbol-name mh) "-mode-hook"))))
    (add-hook hook-name '(lambda () (show-paren-mode 1) (autopair-mode)))
    (add-hook hook-name #'rainbow-delimiters-mode)))

(add-hook 'c++-mode-hook #'rainbow-delimiters-mode)

(global-set-key [f10] 'mark-sexp)
(global-set-key [f9] 'mark-sexp)
(global-set-key (kbd "C-'") 'mark-sexp)
(global-set-key (kbd "M-r") 'raise-sexp)
(global-set-key (kbd "M-t") 'transpose-sexps)
(global-set-key (kbd "M-o") 'oneline)
(global-set-key (kbd "M-m") 'mark-sexp)
(global-set-key (kbd "M-b") 'paredit-forward-barf-sexp)
(global-set-key (kbd "M-s") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "M-j") 'paredit-join-sexps)
(global-set-key (kbd "M-J") 'paredit-split-sexp)
;; (global-set-key (kbd "C-x e") 'lisp-eval-last-sexp) ;; default

;; Nice for debugging lisp in inferior-lisp.
(global-set-key [Scroll_Lock down] 'paredit-forward-down)  ; 'lisp-eval-last-sexp
(global-set-key [Scroll_Lock up] 'paredit-backward-up)
(global-set-key [Scroll_Lock right] 'forward-sexp)
(global-set-key [Scroll_Lock left] 'backward-sexp)

(defun pr (beg end)
  (interactive "r")
  (indent-rigidly beg end 4))
(defun pl (beg end)
  (interactive "r")
  (indent-rigidly beg end -4))

(setq Buffer-menu-name-width 40)

(custom-set-variables
 '(c-offsets-alist (quote ((innamespace . 0))))
 '(rainbow-delimiters-max-face-count 5))
(if 1
    (custom-set-faces
     '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "light green"))))
     '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "deep sky blue"))))
     '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "medium orchid"))))
     '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "orange red"))))
     '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "IndianRed1")))))
  (custom-set-faces
     '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray21"))))
     '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray31"))))
     '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray51"))))
     '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray71"))))
     '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray91"))))
     '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray81"))))
     '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray61"))))
     '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray41"))))
     '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray21"))))))

(fset 'save-and-run-in-other
      [?\C-x ?\C-s ?\C-x ?o ?\M-> ?\M-p return ?\C-x ?o])
(global-set-key (kbd "C-7") 'save-and-run-in-other)

(global-set-key [f3] 'rainbow-delimiters-mode)

(global-set-key [f6] 'save-buffer)
(global-set-key [f5] 'other-window)
