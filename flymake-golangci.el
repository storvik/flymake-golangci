;;; flymake-golangci.el -- Flymake checker for golangci linter -*- lexical-binding: t -*-

;; Author: Petter Storvik
;; URL: https://github.com/storvik/flymake-golangci
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:
;;; Code:

(defgroup flymake-golangci nil
  "Flymake support for golangci."
  :prefix "flymake-golangci-"
  :tag "flymake-golangci"
  :group 'flymake)

(defcustom flymake-golangci-executable "golangci-lint"
  "Path to golangci-lint executable."
  :group 'flymake-golangci
  :type 'string)

(defcustom flymake-golangci-args nil
  "Flags sent to golangci."
  :group 'flymake-golangci
  :type '(list string))

(defvar flymake-golangci--match-regex "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\) \\(([A-Z0-9]+)\\)")

(defun flymake-golangci--check ()
  "Check buffer using golangci."
  (unless (executable-find flymake-golangci-executable)
    (error "Cannot find golangci-lint, is it installed?"))
  (let ((source-buffer (current-buffer))
        (buffer-content (buffer-substring-no-properties (point-min) (point-max)))
        (errors '()))
    (with-temp-buffer
      (insert buffer-content)
      ;; TODO: Must combine args and possibly look for a config file here.
      ;; TODO: Should probably check if this can be done async or something?
      (apply #'call-process-region (point-min) (point-max) flymake-golangci-executable t t nil '("run"))
      (goto-char (point-min))
      (while (search-forward-regexp flymake-golangci--match-regex (point-max) t)
        (when (match-string 2)
          (let* ((line (string-to-number (match-string 2)))
                 (col (string-to-number (match-string 3)))
                 (msg (match-string 4))
                 (linter (match-string 5))
                 (description (format "golangci-lint %s: %s" linter msg))
                 (region (flymake-diag-region source-buffer line col))
                 (dx (flymake-make-diagnostic source-buffer (car region) (cdr region)
                                              :error description)))
            (add-to-list 'errors dx)))))
    errors))

;;;###autoload
(defun flymake-golangci-load ()
  "Adds golangci to `flymake-diagnostic-functions'."
  (interactive)
  (add-hook 'flymake-diagnostic-functions #'flymake-golangci--run-checker nil t))

(defun flymake-golangci--run-checker (report-fn &rest _args)
  "Run checker using REPORT-FN."
  (funcall report-fn (flymake-golangci--check)))
