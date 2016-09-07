(defun lech/init-configs ()
  ;; general
  (setq global-linum-mode t)

  ;; shell
  (setq comint-prompt-read-only nil)
  (setq comint-scroll-to-bottom-on-input t)

  ;; boogie-friends
  ;; (setq flycheck-dafny-executable "PATH-TO-Dafny.exe")
  ;; (setq flycheck-boogie-executable "PATH-TO-BOOGIE")
  ;; (setq flycheck-z3-smt2-executable "/usr/local/bin/z3")
  ;; (setq flycheck-inferior-dafny-executable "PATH-TO-DafnyServer.exe") ;; Optional
  ;; (setq boogie-friends-profile-analyzer-executable "PATH-TO-Z3-AXIOM-PROFILER") ;; Optional
)

(defun lech/init-keys ()
  ;; change buffer to helm-mini
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-h v") 'customize-variable-other-window)

  ;; change comment-dwim to comment-dwim-2
  (global-set-key (kbd "M-;") 'comment-dwim-2)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT SPACELINE

(require 'spaceline-segments)

;; reuse code from spaceline-config.el
(defun tddsg--create-spaceline-theme (left second-left &rest additional-segments)
  "Convenience function for the spacemacs and emacs themes."
  ;; (require 'latex-preview-pane)
  (spaceline-install 'tddsg
                     `(,left
                       anzu
                       auto-compile
                       ,second-left
                       major-mode
                       (process :when active)
                       ((flycheck-error flycheck-warning flycheck-info)
                        :when active)
                       (minor-modes :when active)
                       (mu4e-alert-segment :when active)
                       (erc-track :when active)
                       (version-control :when active)
                       (org-pomodoro :when active)
                       (org-clock :when active)
                       nyan-cat)
                     `(which-function
                       (python-pyvenv :fallback python-pyenv)
                       (battery :when active)
                       selection-info
                       input-method
                       ((buffer-encoding-abbrev))
                       (global :when active)
                       ,@additional-segments
                       buffer-position
                       hud))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-tddsg)))))

;;; used for setting pdf-view page
;;; FIXME: to be removed when spaceline is updated
(declare-function pdf-view-current-page 'pdf-view)
(declare-function pdf-cache-number-of-pages 'pdf-view)

(defun tddsg--pdfview-page-number ()
  (format "(%d/%d)"
          (eval (pdf-view-current-page))
          (pdf-cache-number-of-pages)))

(spaceline-define-segment line-column
  "The current line and column numbers, or `(current page/number of pages)`
in pdf-view mode (enabled by the `pdf-tools' package)."
  (if (eq 'pdf-view-mode major-mode)
      (tddsg--pdfview-page-number)
    "%l:%2c"))


(defun tddsg--create-spaceline-final (&rest additional-segments)
  "Install the modeline used by Spacemacs.
ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (require 'spaceline)
  (require 'pdf-view)
  (apply 'tddsg--create-spaceline-theme
         '((persp-name
            workspace-number
            window-number)
           :fallback evil-state
           :separator "|"
           :face highlight-face)
         '(buffer-modified
           point-position
           line-column
           ;; buffer-size
           buffer-id
           remote-host)
         additional-segments))

(dolist (s '((tddsg-face-unmodified "SteelBlue3" "Unmodified buffer face.")
             (tddsg-face-modified "DarkGoldenrod2" "Modified buffer face.")
             (tddsg-face-read-only "SteelBlue3" "Read-only buffer face.")))
  (eval `(defface, (nth 0 s)
           `((t (:background ,(nth 1 s)
                             :foreground "#3E3D31"
                             :inherit 'mode-line)))
           ,(nth 2 s)
           :group 'tddsg)))


(defun tddsg--spaceline-highlight-face ()
  "Set the highlight face depending on the buffer modified status.
Set `spaceline-highlight-face-func' to
`tddsg--spaceline-highlight-face' to use this."
  (cond
   (buffer-read-only 'tddsg-face-read-only)
   ((buffer-modified-p) 'tddsg-face-modified )
   (t 'tddsg-face-unmodified)))


(defun lech/init-spaceline ()
  (setq spaceline-highlight-face-func 'tddsg--spaceline-highlight-face)
  (tddsg--create-spaceline-final))
