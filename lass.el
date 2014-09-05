;; This file is a part of LASS
;; (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
;; Author: Nicolas Hafner <shinmera@tymoon.eu>

(defun lass-compile-current ()
  (interactive)
  (or
   (when (and (slime-connected-p)
              (or (slime-eval '(cl:not (cl:null (cl:find-package :lass))))
                  (and (slime-eval '(cl:not (cl:null (cl:find-package :ql))))
                       (slime-eval '(ql:quickload :lass)))))
     (message "LASS compiled to %s" (slime-eval `(uiop:native-namestring (lass:generate (uiop:parse-native-namestring ,(buffer-file-name)))))))
   (message "LASS compiled. %s" (shell-command-to-string (format "lass %s" (shell-quote-argument (buffer-file-name)))))))

(define-derived-mode lass-mode common-lisp-mode
  "LASS" "Mode with auto-compiling for LASS files."
  (add-hook 'after-save-hook 'lass-compile-current nil t))

(add-to-list 'auto-mode-alist '("\\.lass\\'" . lass-mode))

(provide 'lass)
