(setq load-path (cons  "c:/erlang/erl5.8.4/lib/tools-2.6.6.4/emacs" load-path))
      (setq erlang-root-dir "c:/erlang/erl5.8.4")
      (setq exec-path (cons "c:/erlang/erl5.8.4/bin" exec-path))
      (require 'erlang-start)	  
	  (require 'erlang-flymake)
	  (require 'erlang-eunit)


(let ((distel-dir "~/emacs-config/distel/elisp"))
  (unless (member distel-dir load-path)
    ;; Add distel-dir to the end of load-path
    (setq load-path (append load-path (list distel-dir)))))
(require 'distel)
(distel-setup)

(add-hook 'erlang-mode-hook
     (lambda ()
        (setq inferior-erlang-machine-options 
			'(
			"-sname" "emacs" 
			"-pa" "apps/*/ebin" 
			;"-boot" "start_sasl"
			))
	  (imenu-add-to-menubar "imenu")))
	  	  
(provide 'erlang-config)


;; Scraps
;; This would be really nice (it provides dialyzer hints with flymake) 
;; but it doesn't work (for me) on Windows. The escript causes permission denied
;;(require 'flymake)
;;(defun flymake-erlang-init ()
;;	(let* ((temp-file (flymake-init-create-temp-buffer-copy
;;		'flymake-create-temp-inplace))
;;	(local-file (file-relative-name temp-file
;;		(file-name-directory buffer-file-name))))
;;	(list "~/emacs-config/check-erlang.erl" (list local-file))))
;;(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))
;;(setq flymake-log-level 3)