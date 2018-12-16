;;; periodic-commit-minor-mode-test.el --- Tests for PCMM
;;; Commentary:

;;; Code:

(ert-deftest commit-unnamed-file ()
  "You cannot commit a file with no filename."
  (let ((buf (get-buffer-create "no filename")))
    (with-current-buffer buf
      (should-error (pcmm-commit)))
    (kill-buffer buf)))

(ert-deftest commit-untracked-file-without-commit-all ()
  "You cannot commit when there are untracked files and
`pcmm-commit-all' is disabled."
  (with-proj "project" t
             (write-region "TEST CONTENT" nil pcmmt-file)
             (with-buffer-visiting-then-kill
              pcmmt-file
              (setq-local pcmm-commit-all nil)
              (should-error (pcmm-commit)))))

(ert-deftest commit-untracked-with-commit-all ()
  "With `pcmm-commit-all' enabled, commit everything always."
  (with-proj "project" t
             (write-region "TEST CONTENT" nil pcmmt-file)
             (with-buffer-visiting-then-kill
              pcmmt-file
              (setq-local pcmm-commit-all t)
              (should (pcmm-commit)))))

(ert-deftest commit-in-unsaved-buffer-fails ()
  "You cannot commit when the current buffer has no file name."
  (with-proj "project"
             (with-current-buffer (get-buffer-create "empty buffer")
               (should-error (pcmm-commit)))))

(ert-deftest auto-commit-respects-frequency ()
  (with-proj "project" t
             (with-buffer-visiting-then-kill
              pcmmt-file
              (goto-char (point-max))
              (insert "new changes")
              (save-buffer)
              (setq pcmm-commit-frequency 100)
              (forge-log pcmmt-metafile 0)
              (should (equal nil (pcmm--commit)))

              (setq pcmm-commit-frequency 100)
              (forge-log pcmmt-metafile -200)
              (should (equal t (pcmm--commit))))))

(provide 'periodic-commit-minor-mode-test)
;;; periodic-commit-minor-mode-test.el ends here
