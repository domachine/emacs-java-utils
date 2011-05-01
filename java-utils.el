(require 'eclipse-exporter)

(defvar java-project-path
  nil
  "The name of the java project.")

(defvar java-project-src-folder
  "src"
  "The folder name to store the source files in (within project directory).")

(defvar java-project-packages
  nil
  "The list with the packages of the java project.")

(defvar java-project-classes
  nil
  "The list with the classes of the java project")

;; (defvar java-project-run-command
;;   "")

(defun java-project-save ()
  (interactive)

  (unless java-project-buffer
    (error "No java project opened"))

  (with-current-buffer java-project-buffer
    (erase-buffer)
    (save-buffer))

  (print `(setq java-project-path ,java-project-path)
         java-project-buffer)

  (print `(setq java-project-packages (quote ,java-project-packages))
         java-project-buffer)

  (print `(setq java-project-classes (quote ,java-project-classes))
         java-project-buffer))

(defun java-project-read-class-path ()
  (list (completing-read "class-package: "
                         java-project-packages)
        (completing-read "class-name: "
                         java-project-classes)))

(defun java-project-open (project-path)
  "Open an existing or empty project."

  (interactive "DProject-path: ")
  (unless (file-directory-p project-path)
    (signal 'wrong-type-argument '(file-directory-p project-path)))

  (setq project-path (expand-file-name project-path))

  (let ((init-file (concat project-path "/project.el")))
    (when (file-regular-p init-file)
      (load-file init-file))

    (when (and (boundp 'java-project-buffer) java-project-buffer)
      (when (buffer-modified-p java-project-buffer)
        (when (yes-or-no-p "Open project unsaved. Save? ")
          (save-buffer))))

    (setq java-project-buffer (find-file-noselect init-file))

    (with-current-buffer java-project-buffer
      (setq java-project-path (file-name-directory (buffer-file-name))))))

(defun java-project-new-package (package-name)
  "Creates a new package in the active project."

  (interactive
   (if java-project-path
       (list (completing-read "Package-name: " java-project-packages))
     (error "No project-path specified"))) 

  (unless (stringp package-name)
    (signal 'wrong-type-argument '(stringp package-name)))

  (let ((package-parts (split-string package-name "\\." t))
        (package-directory (concat java-project-path "/" java-project-src-folder))
        (package-created nil))

    (unless (file-directory-p package-directory)
      (make-directory package-directory))

    (dolist (dir package-parts)
      (setq package-directory (concat package-directory "/" dir))
      (if (file-directory-p package-directory)
          (setq package-created nil)
        (make-directory package-directory)
        (setq package-created t)))

    (when package-created
      (setq java-project-packages (cons package-name java-project-packages))
      (java-project-save))

    (message "Package `%s' created in `%s'" package-name java-project-path)
    package-directory))

(defun java-project-open-class (package class-name)
  (interactive
   (list (completing-read "Package-name: " java-project-packages)
         (completing-read "Class-name: " java-project-classes)))

  (message "%s" load-file-name)
  (let* ((lib-dir (file-name-directory
                   "/home/dominik/.local/share/emacs/site-lisp/java-utils.el"))

         (class-template (concat lib-dir "/class-template.tmpl"))
         (package-parts (split-string package "\\." t))
         (rel-top-level "")
         (source-directory (java-project-new-package package))
         (class-file (concat source-directory "/" class-name ".java"))
         (rel-package-path (replace-regexp-in-string "\\." "/" package))
         (package-stmt "")
         (package-class ""))

    (when (and package (not (string-equal package "")))
      (setq package-stmt (format "package %s;" package)))

    (let ((i 0))
      (dolist (v package-parts)
        (when (> i 0)
          (setq rel-top-level (concat rel-top-level "/")))

        (setq rel-top-level (concat rel-top-level ".."))
        (setq i (1+ i))))

    (if (and package (not (string-equal package "")))
        (setq package-class (concat rel-package-path "/" class-name ".java"))
      (setq package-class (concat class-name ".java")))

    (if (file-regular-p class-file)
        (with-current-buffer (find-file-noselect class-file)
          (c-set-style "java")
          (set-window-buffer (selected-window) (current-buffer)))
      (with-current-buffer (find-file-noselect class-file)
        (erase-buffer)
        (insert-file class-template)
        (beginning-of-buffer)
        (make-local-variable 'compile-command)
        (make-local-variable 'run-command)

        ;; Generate compile and run commands.
        (setq compile-command (format "%sjavac %s"
                                      (if (and rel-top-level
                                               (not (string-equal rel-top-level "")))
                                          (concat "cd " rel-top-level " ; ")
                                        "")
                                      package-class)
              run-command (format "%sjava %s"
                                  (if (and rel-top-level
                                           (not (string-equal rel-top-level "")))
                                      (concat "cd " rel-top-level " ; ")
                                    "")
                                  (concat package "." class-name)))

        (while (search-forward-regexp "\\${\\([a-zA-Z0-9_]+\\)}" nil t)
          (cond ((string-equal (match-string 1) "CLASS")
                 (replace-match class-name t))
                ((string-equal (match-string 1) "COMPILE_COMMAND")
                 (replace-match compile-command t))

                ((string-equal (match-string 1) "RUN_COMMAND")
                 (replace-match (format "%sjava %s"
                                        run-command t)))

                ((string-equal (match-string 1) "PACKAGE_STMT")
                 (replace-match package-stmt t))))

        (beginning-of-buffer)
        (when (search-forward "${CURSOR}" nil t)
          (replace-match ""))

        (c-set-style "java")
        (setq java-project-classes (cons class-name java-project-classes))
        (java-project-save)
        (set-window-buffer (selected-window) (current-buffer))))))

(defun java-project-close ()
  (interactive)

  (unless java-project-path
    (error "No project open"))

  (when (and (boundp 'java-project-buffer) java-project-buffer)
    (kill-buffer java-project-buffer))

  (setq java-project-path nil
        java-project-buffer nil
        java-project-packages nil
        java-project-classes nil))

(defun java-project-export ()
  (interactive)

  (unless java-project-path
    (error "No project open"))

  (eclipse-export-project java-project-path))
;;(setq java-project-path (expand-file-name "~/Workspace/PIStuff/PvsBlatt01"))

(defun java-project-xephyr-run-class ()
  (interactive)

  (let* ((class-path (java-project-read-class-path))
         (class-package (car class-path))
         (class-name (car (cdr class-path))))

   (unless (boundp 'run-command)
      (error "run-command not bound."))

    ;; Be sure that a Xephyr server is running, otherwise
    ;; launch one.
    (unless (get-process "Xephyr")
      (start-process "Xephyr" (with-current-buffer (get-buffer-create "*Xephyr*")
                                (erase-buffer)
                                (current-buffer))
                     "Xephyr" ":1" "-ac" "-br"
                     "-reset" "-terminate")
      (let ((process-environment (cons "DISPLAY=:1" process-environment)))
        (start-process "OpenBox" nil "openbox")))

    (async-shell-command (concat "DISPLAY=:1 " run-command))))


(provide 'java-utils)
