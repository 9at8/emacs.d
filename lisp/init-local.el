;;; package --- summary

;;; Commentary:

;;; Code:

(defun load-init ()
  "Set some things on initialization.
\(load-init)"
  ;; set font size which doesn't hurt my eyes
  (set-face-attribute 'default nil :font "Hack-12")
  (set-window-margins nil 0)
  ;; always show line numbers
  (global-display-line-numbers-mode)
  ;; always wrap words
  (global-visual-line-mode)
  ;; disable fringes - something like boundaries
  (set-fringe-style 0))

(defun load-initd (frame)
  "Set the font using set-font on the FRAME.
\(set-fontd)"
  (select-frame frame)
  (load-init))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-initd)
  (load-init))

;;; highlight indent guides
(require-package 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'column)
(setq highlight-indent-guides-responsive 'top)

;;; Add some spacing between lines
(setq-default line-spacing 5)

;;; Use telephone line
(require-package 'telephone-line)

(setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
        (nil    . (telephone-line-minor-mode-segment
                   telephone-line-buffer-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))

(telephone-line-mode 1)

;;; Neotree
(require-package 'neotree)
(require-package 'all-the-icons)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq neo-window-fixed-size nil)

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(setq mac-option-modifier 'meta)
(global-set-key (kbd "s-b") 'neotree-project-dir)

;;; Atom one dark theme
(require-package 'atom-one-dark-theme)
(require-package 'dracula-theme)
;; (load-theme 'atom-one-dark t)
(load-theme 'dracula)

;;; Show file diffs
(add-hook 'after-init-hook 'diff-hl-margin-mode)

;;; C things
(setq c-default-style "k&r"
      c-basic-offset 4
      tab-width 4)

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++17")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-c/c++-gcc-executable "gcc-8")))

;;; Make sure ripgrep is installed
(global-set-key (kbd "s-s") 'projectile-ripgrep)
(require-package 'ripgrep)
(require-package 'projectile-ripgrep)

;;; Footer:
(provide 'init-local)

;;; init-local.el ends here
