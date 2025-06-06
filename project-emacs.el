(use-package ace-window
    :ensure
    :config
(setq aw-dispatch-always t)
    :bind
    (("M-O" . #'ace-window)
     ("M-o" . #'rlr/quick-window-jump)))
