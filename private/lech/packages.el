;;; packages.el --- lech layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Le Ton Chanh <chanhle@mint-chanhle>
;; URL: https://github.com/letonchanh
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lech-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lech/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lech/pre-init-PACKAGE' and/or
;;   `lech/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst lech-packages
  '(;; Packages for emacs display
    helm
    ;; Packages for editor
    comment-dwim-2
    ;; Packages for LaTeX
    auctex
    ; latex-extra
    pdf-tools
    ;; latex-preview-pane
    ;; Package for z3
    boogie-friends
    (songbird :location local))

  "The list of Lisp packages required by the lech layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


;;; INIT PACKAGES

(defun lech/init-comment-dwim-2 ()
  (require 'comment-dwim-2))

(defun lech/init-pdf-tools ()
  (require 'pdf-tools)
  (pdf-tools-install)
  (custom-set-variables
   '(pdf-view-midnight-colors (quote ("#D3D3D3" . "#292B2E"))))
  (defun my-pdf-view-hook ()
    (if (not (bound-and-true-p pdf-view-midnight-minor-mode))
        (pdf-view-midnight-minor-mode)))
  (add-hook 'pdf-view-mode-hook 'my-pdf-view-hook))

;; (defun lech/init-latex-preview-pane ()
;;   (require 'latex-preview-pane))

(defun lech/post-init-auctex ()
  (require 'tex)
  ;; (add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t))
  (custom-set-variables
   '(TeX-save-query nil)
   '(TeX-source-correlate-method (quote synctex))
   '(TeX-source-correlate-mode t)
   '(TeX-source-correlate-start-server t)
   '(LaTeX-command "latex --synctex=1")
   '(TeX-view-program-list
     (quote (("pdf-tools" "TeX-pdf-tools-sync-view"))))
   '(TeX-view-program-selection
     (quote ((engine-omega "dvips and gv")
             (output-dvi "xdvi")
             (output-pdf "pdf-tools")
             (output-html "xdg-open"))))))

(defun lech/init-boogie-friends ()
  (require 'boogie-friends)
  (setq flycheck-z3-smt2-executable "/usr/local/bin/z3"))

(defun tddsg/post-init-tuareg ()
  (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

  (require 'ocp-indent)
  (require 'merlin))

;;; LOCAL PACKAGES

(defun lech/init-songbird ()
  (require 'songbird)
  (add-to-list 'auto-mode-alist '("\\.sb\\'" . songbird))
  (add-to-list 'auto-mode-alist '("\\.ss\\'" . songbird))
  (add-to-list 'auto-mode-alist '("\\.slk\\'" . songbird))
  (defun my-songbird-hook ()
    ;; customize syntax table for slurping/barfing parentheses
    (dolist (symbol (list ?. ?, ?\; ?: ?+ ?- ?@ ?! ?> ?<))
      (modify-syntax-entry symbol "'" songbird-syntax-table)))
  (add-hook 'songbird-hook 'my-songbird-hook 'append))

;;; packages.el ends here
