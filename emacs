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
 '(inhibit-startup-screen t)
 '(merlin-command "ocamlmerlin")
 '(package-selected-packages (quote (company ocp-indent merlin pkgbuild-mode tuareg)))
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
 '(default ((t (:background "black" :foreground "grey" :height 110 :family "Consolas")))))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key "\C-xt" 'delete-trailing-whitespace)

;; +-----------------------------------------------------------------+
;; | Dev                                                             |
;; +-----------------------------------------------------------------+

(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'tuareg-mode-hook 'ocp-setup-indent)

(defun set-compile-command ()
  (interactive)
  (set (make-local-variable 'compile-command)
       (format "cd %s && make"
               (file-relative-name
                (locate-dominating-file buffer-file-name "Makefile")
                (file-name-directory buffer-file-name)))))

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
