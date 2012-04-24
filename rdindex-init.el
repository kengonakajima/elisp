(autoload 'rdindex-describe-function "rdindex" nil t)
(autoload 'rdindex-describe-function-2 "rdindex" nil t)
(autoload 'ruby-complete-method "rdindex" nil t)

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map "\C-q\C-f" 'rdindex-describe-function-2)
            (define-key ruby-mode-map "\C-q\C-c" 'rdindex-describe-function-briefly-2)
            (define-key ruby-mode-map "\M-\C-i" 'ruby-complete-method)            
            ))
(add-hook 'inferior-ruby-mode-hook
          (lambda ()
            (define-key inferior-ruby-mode-map "\C-q\C-f" 'rdindex-describe-function-2)
            (define-key inferior-ruby-mode-map "\C-q\C-c" 'rdindex-describe-function-briefly-2)
            (define-key inferior-ruby-mode-map "\M-\C-i" 'ruby-complete-method)
            ))
