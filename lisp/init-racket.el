;;; init-racket.el --- Support for the Racket language
;;; Commentary:
;;; Code:

(require-package 'racket-mode)
(require-package 'geiser)
(require-package 'ac-geiser)

;; (setq geiser-active-implementations '(racket))

;; (let ((conf (current-window-configuration)))
;;   (run-geiser)
;;   (set-window-configuration conf))

(provide 'init-racket)
;;; init-racket.el ends here
