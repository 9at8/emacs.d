;;; init-sml.el --- Support for Standard ML
;;; Commentary:
;;; Code:

(require-package 'sml-mode)

(add-hook 'sml-mode-hook 'enable-paredit-mode)

(provide 'init-sml)
;;; init-sml.el ends here
