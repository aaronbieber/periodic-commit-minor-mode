;;; periodic-commit-minor-mode-test.el --- Tests for PCMM
;;; Commentary:

;;; Code:

(ert-deftest commit-in-non-proj-dir-fails ()
  "Attempting to auto-commit in a directory that is not a project
repository fails."
  (with-proj "project" nil
             (should-error (pcmm-commit))))

(ert-deftest commit-untracked-file-fails ()
  "You cannot commit when there are untracked files and
`pcmm-commit-all' is disabled."
  (with-proj "project" t
             (write-region "TEST CONTENT" nil pcmmt-file)
             (let ((test-buf (find-file-noselect pcmmt-file)))
               (with-current-buffer test-buf
                 (setq-local pcmm-commit-all nil)
                 (should-error (pcmm-commit))))))

(ert-deftest commit-in-unsaved-buffer-fails ()
  "You cannot commit when the current buffer has no file name."
  (with-proj "project"
             (with-current-buffer (get-buffer-create "empty buffer")
               (should-error (pcmm-commit)))))

(provide 'periodic-commit-minor-mode-test)
;;; periodic-commit-minor-mode-test.el ends here
