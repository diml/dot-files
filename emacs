;; -*- emacs-lisp -*-

;; .emacs
;; ------
;; Copyright : (c) 2017, Jeremie Dimino <jeremie@dimino.org>
;; Licence   : BSD3

;; +-----------------------------------------------------------------+
;; | General stuff                                                   |
;; +-----------------------------------------------------------------+

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.backup"))))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-scroll-output (quote first-error))
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(merlin-command "ocamlmerlin")
 '(ocamlformat-enable (quote disable-outside-detected-project))
 '(package-selected-packages
   (quote
    (utop rg ack markdown-mode cygwin-mount company)))
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values (quote ((eval set-compile-command))))
 '(save-abbrevs (quote silently))
 '(savehist-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(caml-types-expr-face ((t (:background "forest green"))))
 '(font-lock-comment-face ((t (:foreground "green3"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "pale goldenrod"))))
 '(font-lock-function-name-face ((t (:foreground "LightSkyBlue" :weight semi-bold))))
 '(help-argument-name ((t (:background "gray25"))))
 '(mouse ((t (:background "purple"))))
 '(tuareg-font-lock-governing-face ((t (:inherit font-lock-keyword-face :foreground "orange" :weight bold))))
 '(tuareg-font-lock-operator-face ((t (:inherit font-lock-keyword-face :foreground "lightblue"))))
 '(whitespace-trailing ((t (:background "pink" :weight bold)))))

(defun jane-street-dark-theme ()
  "Dark theme imported from Jane Street elisp code"
  (interactive)
  (custom-set-faces
   '(caml-types-expr-face ((t (:background "forest green"))))
   '(font-lock-comment-face ((t (:foreground "green3"))))
   '(font-lock-doc-face
     ((t (:inherit font-lock-comment-face :foreground "pale goldenrod"))))
   '(font-lock-function-name-face ((t (:foreground "LightSkyBlue" :weight semi-bold))))
   '(help-argument-name ((t (:background "gray25"))))
   '(mouse ((t (:background "purple"))))
   '(tuareg-font-lock-governing-face
     ((t (:inherit font-lock-keyword-face :foreground "orange" :weight bold))))
   '(tuareg-font-lock-operator-face
     ((t (:inherit font-lock-keyword-face :foreground "lightblue"))))
   '(whitespace-trailing ((t (:background "pink" :weight bold))))))

(jane-street-dark-theme)

(cond
 ((memq system-type '(cygwin windows-nt))
  (set-frame-font "Consolas-11"))
 ((eq system-type 'darwin)
  (custom-set-variables
   '(tool-bar-mode nil))
  ;; So that Right-Option-3 produces a #
  (setq ns-right-alternate-modifier (quote none)))
 (t
  (custom-set-variables
   '(tool-bar-mode nil)
   '(menu-bar-mode nil))))

(if (display-graphic-p)
    (progn
      (set-foreground-color "#eeedf0")
      (set-background-color "#012456")
      (set-cursor-color "yellow"))
  (setq frame-background-mode 'dark)
  (custom-set-variables
    '(menu-bar-mode nil)))

;; Currently, there are some glitches when using parenthesis matching
;; with stdreplay, so we disable it when stdreplay is in use
(when (getenv "STDREPLAY") (setq blink-matching-paren nil))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key "\C-xt" 'delete-trailing-whitespace)

;; +-----------------------------------------------------------------+
;; | Dev                                                             |
;; +-----------------------------------------------------------------+

;; Get emacs stuff from opam
(let ((prefix (getenv "OPAM_SWITCH_PREFIX")))
  (when prefix
    (add-to-list 'load-path (concat prefix "/share/emacs/site-lisp"))))

(require 'line-up-words nil t)
(global-set-key "\C-ca" 'line-up-words)

(require 'tuareg nil t)
(require 'dune nil t)
(require 'merlin nil t)
(require 'ocp-indent  nil t)
(require 'ocamlformat nil t)

(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'tuareg-mode-hook 'company-mode)

(defun set-compile-command ()
  (interactive)
  (let* ((dir (locate-dominating-file buffer-file-name "Makefile")))
    (when dir
      (set (make-local-variable 'compile-command)
           (format "cd %s && make"
                   (file-relative-name
                    dir
                    (file-name-directory buffer-file-name)))))))

(defun my-tuareg-mode-hook ()
  (set-compile-command)

  ;; ocamlformat stuff
  (define-key merlin-mode-map (kbd "C-M-<tab>") 'ocamlformat)
  (add-hook 'before-save-hook 'ocamlformat-before-save)

  ;; company stuff
  (define-key merlin-mode-map (kbd "M-.") 'company-complete))

(add-hook 'tuareg-mode-hook 'my-tuareg-mode-hook)
(add-hook 'dune-mode-hook   'set-compile-command)

(require 'whitespace)
(setq whitespace-style '(face tabs lines-tail trailing))
(global-whitespace-mode t)

;; +-----------------------------------------------------------------+
;; | Utils                                                           |
;; +-----------------------------------------------------------------+

(defun strlen ()
  "Calcul length of string under cursor."
  (interactive)
  (save-excursion
    (save-restriction
          (widen)
          (let ((start-pos (search-backward "\"")))
            (forward-char 1)
            (message "string length: %d" (- (search-forward "\"") start-pos 2))))))

(defun section ()
  "Insert a section comment"
  (interactive)
  (goto-char (point-at-bol))
  (cond
   ((eq major-mode `emacs-lisp-mode)
    (insert ";; +-----------------------------------------------------------------+\n"
            ";; |                                                                 |\n"
            ";; +-----------------------------------------------------------------+\n"))
   ((eq major-mode `latex-mode)
    (insert "%% +-----------------------------------------------------------------+\n"
            "%% |                                                                 |\n"
            "%% +-----------------------------------------------------------------+\n"))
   ((or (eq major-mode `tuareg-mode) (eq major-mode `coq-mode))
    (insert "(* +-----------------------------------------------------------------+\n"
            "   |                                                                 |\n"
            "   +-----------------------------------------------------------------+ *)\n"))
   ((or (eq major-mode `sh-mode)
        (eq major-mode `makefile-mode)
        (eq major-mode `makefile-gmake-mode)
        (eq major-mode `python-mode))
    (insert "# +------------------------------------------------------------------+\n"
            "# |                                                                  |\n"
            "# +------------------------------------------------------------------+\n"))
   ((or (eq major-mode `c-mode) (eq major-mode `js-mode))
    (insert "/* +-----------------------------------------------------------------+\n"
            "   |                                                                 |\n"
            "   +-----------------------------------------------------------------+ */\n"))
   ((eq major-mode `lua-mode)
    (insert "-- +-----------------------------------------------------------------+\n"
            "-- |                                                                 |\n"
            "-- +-----------------------------------------------------------------+\n"))
   (t
    (insert "+-------------------------------------------------------------------+\n"
            "|                                                                   |\n"
            "+-------------------------------------------------------------------+\n")))
  (previous-line 2)
  (forward-char 5))

;; +-----------------------------------------------------------------+
;; | Local customization                                             |
;; +-----------------------------------------------------------------+

(when (file-exists-p "~/site-lisp/init.el")
  (load-file "~/site-lisp/init.el"))
