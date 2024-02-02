# flymake-golangci

Flymake backend for [golangci-lint](https://github.com/golangci/golangci-lint).

## Installation

I'm using `use-package` and `elpaca` package manager:

```emacs-lisp
(use-package flymake-golangci
  :elpaca (flymake-golangci :host githug :repo "storvik/flymake-golangci")
  :hook ((eglot-managed-mode . flymake-golangci-load) ;; using flymake-golangci with eglot
         (go-mode . flymake-golangci-load)))          ;; using flymake-golangci with go-mode
```

## Thanks to

[flymake-ruff](https://github.com/erickgnavar/flymake-ruff), for inspiration!
