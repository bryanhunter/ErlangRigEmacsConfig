(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
	(setq auto-mode-alist
    (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(provide 'csharp-config)
