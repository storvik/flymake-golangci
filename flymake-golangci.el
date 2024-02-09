;;; flymake-golangci.el -- Flymake checker for golangci linter -*- lexical-binding: t -*-

;; Copyright (C) 2024 Petter Storvik

;; Author: Petter Storvik <petterstorvik@gmail.com>
;; Keywords: linter, tools, go
;; URL: https://github.com/storvik/flymake-golangci
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Flymake backend for golangci-lint linter.
;;
;; Usage:
;;
;; Add backend to go-mode with:
;;
;;   (add-hook 'go-mode #'flymake-golangci-load-backend)
;;
;; Or if using eglot:
;;
;;   (add-hook 'eglot-managed-mode #'flymake-golangci-load-backend)
;;
;; Note, this does not enable flymake or eldoc.
;;

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

(defvar-local flymake-golangci--proc nil)

(defun flymake-golangci (report-fn &rest _args)
  "Flymake backend function for golangci-lint, a linter for Go. "
  (unless (executable-find flymake-golangci-executable)
    (error "Cannot find golangci-lint, is it installed?"))
  (when (process-live-p flymake-golangci--proc)
    (kill-process flymake-golangci--proc))
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      ;; Reset the `flymake-golangci--proc' process to a new process
      (setq
       flymake-golangci--proc
       (make-process
        :name "flymake-golangci" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *flymake-golangci*")
        :command `(,flymake-golangci-executable "run" ,(buffer-file-name source))
        :sentinel
        (lambda (proc _event)
          ;; Check that the process has indeed exited, as it might be simply suspended.
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                ;; Only proceed if `proc' is the same as `flymake-golangci--proc',
                ;; which indicates that `proc' is not an obsolete process.
                (if (with-current-buffer source (eq proc flymake-golangci--proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      ;; Parse the buffer, collect them and call `report-fn'.
                      (cl-loop
                       while (search-forward-regexp
                              flymake-golangci--match-regex
                              nil t)
                       for msg = (format "golangci-lint %s: %s"
                                         (match-string 5)
                                         (match-string 4))
                       for (beg . end) = (flymake-diag-region
                                          source
                                          (string-to-number (match-string 2))
                                          (string-to-number (match-string 3)))
                       when (and beg end)
                       collect (flymake-make-diagnostic source
                                                        beg
                                                        end
                                                        :error
                                                        msg)
                       into diags
                       finally (funcall report-fn diags)))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              ;; Cleanup the temporary buffer used to hold the check's output.
              (kill-buffer (process-buffer proc)))))))
      ;; Send the buffer contents to the process's stdin, followed by EOF.
      (process-send-region flymake-golangci--proc (point-min) (point-max))
      (process-send-eof flymake-golangci--proc))))

;;;###autoload
(defun flymake-golangci-load-backend ()
  (add-hook 'flymake-diagnostic-functions 'flymake-golangci nil t))

(provide 'flymake-golangci)
;;; flymake-golangci.el ends here
