;; This file is a part of LASS
;; (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
;; Author: Nicolas Hafner <shinmera@tymoon.eu>

(defcustom lass-generate-pretty-p nil
  "Whether to generate pretty .lass files."
  :type 'boolean)

(defun lass-compile-current ()
  (interactive)
  (or
   (when (and (and (fboundp 'slime-connected-p) (slime-connected-p))
              (or (slime-eval '(cl:not (cl:null (cl:find-package :lass))))
                  (and (slime-eval '(cl:not (cl:null (cl:find-package :ql))))
                       (slime-eval '(ql:quickload :lass)))))
     (message "LASS compiled to %s"
              (slime-eval `(uiop:native-namestring
                            (lass:generate
                             (uiop:parse-native-namestring ,(buffer-file-name))
                             :pretty ,lass-generate-pretty-p)))))
   (when (and (and (fboundp 'sly-connected-p) (sly-connected-p))
              (or (sly-eval '(cl:not (cl:null (cl:find-package :lass))))
                  (and (sly-eval '(cl:not (cl:null (cl:find-package :ql))))
                       (sly-eval '(ql:quickload :lass)))))
     (message "LASS compiled to %s"
              (sly-eval `(uiop:native-namestring
                            (lass:generate
                             (uiop:parse-native-namestring ,(buffer-file-name))
                             :pretty ,lass-generate-pretty-p)))))
   (message "LASS compiled. %s" (shell-command-to-string (format "lass %s" (shell-quote-argument (buffer-file-name)))))))

(define-derived-mode lass-mode common-lisp-mode
  "LASS" "Mode with auto-compiling for LASS files."
  (add-hook 'after-save-hook 'lass-compile-current nil t))

(add-to-list 'auto-mode-alist '("\\.lass\\'" . lass-mode))

(provide 'lass)
