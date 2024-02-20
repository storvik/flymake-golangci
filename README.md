# flymake-golangci

Flymake backend for [golangci-lint](https://github.com/golangci/golangci-lint).
My go to Go linter! Pun intended.

## Installation

Installation using [straight.el](https://github.com/radian-software/straight.el) or [elpaca](https://github.com/progfolio/elpaca).

```emacs-lisp
(use-package flymake-golangci
  :elpaca (flymake-golangci :host github :repo "storvik/flymake-golangci")             ;; using elpaca
  :straight (flymake-golangci :type git :host github :repo "storvik/flymake-golangci") ;; using straight
  :hook ((eglot-managed-mode . (lambda ()
                                (when (derived-mode-p '(go-mode go-ts-mode))
                                  (flymake-golangci-load-backend)))) ;; using flymake-golangci with eglot
         (go-mode . flymake-golangci-load-backend)                   ;; using flymake-golangci with go-mode
		 (go-ts-mode . flymake-golangci-load-backend)))              ;; using flymake-golangci with go-ts-mode
```

Note that this config does not enable `flymake` or `eldoc` automatically.

## Thanks to

[flymake-ruff](https://github.com/erickgnavar/flymake-ruff), for inspiration!
