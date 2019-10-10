(install-packages (list 'helm-dash
			'javadoc-lookup))
	
;; Java
					;

;; CUSTOM VARIABLES SET THEM UP
;; (jdee-compiler (quote ("javac")))
;;  '(jdee-complete-function (quote jdee-complete-minibuf))
;;  '(jdee-complete-insert-method-signature nil)
;;  '(jdee-help-docsets
;;    (quote
;;     (("nil" "file:///usr/lib/jvm/java-8-openjdk-amd64/docs/api" "1.8")
;;      ("nil" "file:///usr/lib/jvm/java-9-openjdk-amd64/docs/api" "1.9")
;;      (nil "file:///opt/WTK2.5.2/docs/api" "WTK2.5.2"))))
;;  '(jdee-jdk (quote ("1.8")))
;;  '(jdee-jdk-registry
;;    (quote
;;     (("1.8" . "/usr/lib/jvm/java-8-openjdk-amd64")
;;      ("1.9" . "/usr/lib/jvm/java-9-openjdk-amd64"))))
;;  '(jdee-server-dir "~/.emacs.d/elpa/jdee-20170217.22/")
(defun my-java-make-compile ()
  (compile (format "make -k %s" (file-name-base (buffer-file-name)))))

(defun my-java-make-run ()
  (let* ((name (file-name-base (buffer-file-name)))
         (args (read-string (format "make  execute-%s " name))))
    (async-shell-command (format "make execute-%s execargs=\"%s\"" name args)
                   (get-buffer-create "output"))
    (set-window-buffer (car (last (window-list-1))) "output")
    ))

(defun my-java-make-cr ()
  (my-java-make-compile)
  (my-java-make-run))


(defun setup-java-make ()
  (interactive)
  (setup-make-keys)
  (setq-local make-compile-func #'my-java-make-compile)		
  (setq-local make-run-func #'my-java-make-run)
  (setq-local make-compile-run-func #'my-java-make-cr)
  (setq-local make-default-makefile "/home/bpanthi/.emacs.d/default-java-makefile"))

(defvar javame-auto-on nil)

(defun javame-auto ()
  (if javame-auto-on
      (progn
	(message "javame auto off")
	(setq javame-auto-on nil))
    (progn
      (message "javame auto on")
      (setq javame-auto-on t))))
	

(defun javame ()
  (interactive)
  ;; (ajc-load-tag-file (expand-file-name "~/.emacs.d/javame.tag"))
  (javadoc-add-roots "/opt/WTK2.5.2/docs")
  (setq-local make-default-makefile "/home/bpanthi/.emacs.d/default-javame-makefile"))

(defun setup-java-keys ()
  (interactive)
  (local-set-key (kbd "C-.") 'javadoc-lookup)
  (local-set-key (kbd "M-.") 'helm-dash-at-point))

(defun my-java-mode-hook ()
  (require 'javadoc-lookup)
  (javadoc-add-roots "/usr/lib/jvm/default-java/docs/api")
  ;; (require 'ajc-java-complete-config)
  ;; (ajc-load-tag-file (expand-file-name "~/.emacs.d/java_base2.tag"))
  ;; (ajc-java-complete-mode)
  (company-mode nil) 
  (require 'helm-dash)
  (helm-dash-activate-docset "Java_SE8")
  (when javame-auto-on (javame))
  (setup-java-make)
  (setup-java-keys))

 ;;  (require 'key-chord)
 ;;  (key-chord-mode t)
 ;; (key-chord-define-local "  " 'jdee-complete))

(add-hook 'java-mode-hook 'my-java-mode-hook)

