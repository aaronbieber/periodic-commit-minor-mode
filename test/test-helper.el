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
    (cd test-dir)
    (if init (vc-git-command buf 0 nil "init"))
    test-dir))

(defun pcmmt-delete-proj (name)
  "Delete a directory called NAME, basically."
  (let* ((test-dir (expand-file-name name temporary-file-directory)))
    (progn
      (if (file-exists-p test-dir)
          (delete-directory test-dir t)))))

(defmacro with-buffer-visiting-then-kill (filename &rest body)
  "Read FILENAME into a buffer, execute BODY, then kill the buffer."
  `(let ((buf (find-file-noselect ,filename t)))
       (with-current-buffer buf ,@body)
         (kill-buffer buf)))

(defmacro with-proj (name init &rest forms)
  "Create dir NAME, optionally INIT as a Git repo, and evaluate FORMS.

This macro ensures that the directory created is always destroyed,
even if FORMS results in errors."
  (let ((unique-name (concat name "-" (random-string))))
    `(unwind-protect
         (let* (
                (pcmmt-dir (pcmmt-create-proj ,unique-name ,init))
                (pcmmt-file (expand-file-name (concat ,unique-name "-file.txt") pcmmt-dir))
                (pcmmt-metafile (expand-file-name ".pcmm" pcmmt-dir)))
           ,@forms
           pcmmt-dir)
       (pcmmt-delete-proj ,unique-name))))

(defun forge-log (filename offset)
  "Write the current time plus OFFSET to FILENAME."
  (let* ((current-time (string-to-number (format-time-string "%s")))
         (offset-time (+ current-time offset)))
    (write-region (number-to-string offset-time) nil filename)))

(defun random-char ()
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))

(defun random-string ()
  (concat (random-char)
          (random-char)
          (random-char)
          (random-char)
          (random-char)))

(provide 'test-helper)
;;; test-helper.el ends here
