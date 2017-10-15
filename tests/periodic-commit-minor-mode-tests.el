;;; periodic-commit-minor-mode-tests --- Tests for this minor mode -*-lexical-binding-*-
;;; Commentary:
;;; Code:
(require 'periodic-commit-minor-mode)
(require 'magit)
(require 'vc-git)

(defun pcmmt-set-up-dir (&optional init)
  "Set up a repo for testing.

If INIT is non-nil, initialize the directory as a Git repository.

Returns the path to the directory."
  (let* ((test-dir (expand-file-name "pcmm-test" temporary-file-directory)))
    (progn
      (if (file-exists-p test-dir)
          (delete-directory test-dir t))
      (make-directory test-dir)
      (if init
          (vc-git--call "*vc-git-output*" "init" test-dir)))
    test-dir))

(describe "From a plain directory"
  :var (test-dir
        test-filename
        test-buf)

  (before-all
    (setq test-dir (pcmmt-set-up-dir))
    (setq test-filename (expand-file-name "pcmm-test-file" test-dir))
    (setq test-metafile (expand-file-name ".pcmm" test-dir))
    (write-region "" "" test-filename)
    (setq test-buf (find-file-noselect test-filename))
    (if (file-exists-p test-metafile)
        (delete-file test-metafile)))

  (after-all
    (delete-directory test-dir t))

  (it "commit an untracked file"
    (expect 'pcmm-commit :to-throw)))

(describe "From an initialized repository"
  :var (test-dir
        test-filename
        test-buf)

  (before-all
    (setq test-dir (pcmmt-set-up-dir t))
    (setq test-filename (expand-file-name "pcmm-test-file" test-dir))
    (write-region "" "" test-filename)
    (setq test-buf (find-file-noselect test-filename)))

  (after-all
    (delete-directory test-dir t))

  (it "commit an untracked file"
    (expect 'pcmm-commit :to-throw))

  (it "commit a tracked file with no changes"
    (expect
     (lambda ()
       (magit-stage-modified t)
       (pcmm--commit))
     :to-throw)))

(provide 'periodic-commit-minor-mode-tests)
;;; periodic-commit-minor-mode-tests.el ends here
