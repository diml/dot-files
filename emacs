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
 '(merlin-command "ocamlmerlin")
 '(package-selected-packages
   (quote
    (ack markdown-mode cygwin-mount company ocp-indent merlin pkgbuild-mode tuareg)))
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values (quote ((eval set-compile-command))))
 '(save-abbrevs (quote silently))
 '(savehist-mode t)
 '(show-trailing-whitespace t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'tuareg-mode-hook 'ocp-setup-indent)

(defun set-compile-command ()
  (interactive)
  (let* ((dir (locate-dominating-file buffer-file-name "Makefile")))
    (when dir
      (set (make-local-variable 'compile-command)
           (format "cd %s && make"
                   (file-relative-name
                    dir
                    (file-name-directory buffer-file-name)))))))

(add-hook 'tuareg-mode-hook 'set-compile-command)

;; Make company aware of merlin
(with-eval-after-load 'company
 (add-to-list 'company-backends 'merlin-company-backend))

;; Enable company on merlin managed buffers
(add-hook 'merlin-mode-hook 'company-mode)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
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
