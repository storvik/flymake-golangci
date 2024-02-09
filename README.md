# flymake-golangci

Flymake backend for [golangci-lint](https://github.com/golangci/golangci-lint).
My go to Go linter! Pun intended.

## Installation

I'm using `use-package` and `elpaca` package manager:

```emacs-lisp
(use-package flymake-golangci
  :elpaca (flymake-golangci :host githug :repo "storvik/flymake-golangci")
  :hook ((eglot-managed-mode . (lambda ()
                                (when (derived-mode-p 'go-mode)
                                  (flymake-golangci-load-backend)))) ;; using flymake-golangci with eglot
         (go-mode . flymake-golangci-load-backend)))                 ;; using flymake-golangci with go-mode
```

Note that this config does not enable `flymake` or `eldoc` automatically.

## Thanks to

[flymake-ruff](https://github.com/erickgnavar/flymake-ruff), for inspiration!
