;;; test-helper.el --- Configure an ERT test environment

;;; Commentary:

;;; Code:

;; This can likely be removed once pcmm no longer depends on Magit.
(package-initialize)

(defvar pcmm-test-setup-dir
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(add-to-list 'load-path (concat pcmm-test-setup-dir ".."))

(require 'periodic-commit-minor-mode)
(require 'vc)
(require 'vc-git)

(defun pcmmt-create-proj (name &optional init)
  "Create a repo in a temp dir called NAME and optionally run INIT."
  (let* ((test-dir (expand-file-name name temporary-file-directory))
         (buf (get-buffer-create "*vc-git-output*")))
    (make-directory test-dir)
    (if init (vc-git-command buf nil test-dir "init"))
    test-dir))

(defun pcmmt-delete-proj (name)
  "Delete a directory called NAME, basically."
  (let* ((test-dir (expand-file-name name temporary-file-directory)))
    (progn
      (if (file-exists-p test-dir)
          (delete-directory test-dir t)))))

(defmacro with-proj (name init &rest forms)
  "Create dir NAME, optionally INIT as a Git repo, and evaluate FORMS.

This macro ensures that the directory created is always destroyed,
even if FORMS results in errors."
  `(unwind-protect
       (let* ((pcmmt-dir (pcmmt-create-proj ,name ,init))
              (pcmmt-file (expand-file-name ,name pcmmt-dir))
              (pcmmt-metafile (expand-file-name ".pcmm" pcmmt-dir)))
         ,@forms
         pcmmt-dir)
     (pcmmt-delete-proj ,name)))

(provide 'test-helper)
;;; test-helper.el ends here
