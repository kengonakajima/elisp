;;; rdindex.el $Id: rdindex.el,v 1.1 2001/01/31 01:37:04 rubikitch Exp rubikitch $
(require 'cl)

(defvar rdindex-completion-table nil "Completion table for rdindex-describe-function.")
(defvar rdindex-method-table nil "")
(defvar rdindex-method-klass-table nil "")
(defvar rdindex-method-klass-hash nil "")
(defvar rdindex-completion-table nil "")
(defvar rdindex-command "rdindex" "The command name of `rdindex'.")
(defvar rdindex-display-function 'rdindex-display-buffer "Display function of rdindex-describe-function.")
(defvar rdindex-display-read-only nil "If non-nil, make the RD buffer read-only.")

(defun rdindex-describe-function (func)
  "Search the RD documentation of FUNC."
  (interactive (list (rdindex-read-function "Ruby function: " rdindex-completion-table)))
  (let* ((pair (rdindex-search-method func))
         (ffunc (progn (string-match "\\(::\\|#\\|\\.\\)\\(.+\\)$" func) (match-string 2 func)))
         (file (car pair))
         (line (cdr pair))
         (buf (find-file-noselect file))
         (proc (lambda ()
                 (and rdindex-display-read-only (setq buffer-read-only t))
                 (goto-line line)
                 (search-forward ffunc)))
         window curwin)
    (cond ((setq curwin (selected-window)
                 window (get-buffer-window buf))
           (select-window window)
           (push-mark (point) nil t)
           (funcall proc)
           (recenter 0)
           (select-window curwin))
          (t
           (set-buffer buf)
           (funcall proc)
           (funcall rdindex-display-function buf)))))

(defun rdindex-read-function-2 ()
   (let* ((m (progn
               (or rdindex-method-table (rdindex-make-completion-table))
               (rdindex-read-string-with-default )))
          ;(klass-table (gethash m rdindex-method-klass-hash))
          (klass-table (cdr (assoc m rdindex-method-klass-hash)))
          (k (if (= (length klass-table) 1)
                 (car (car klass-table))
               (rdindex-read-function "Class: " klass-table))))
     (concat  k m)))


(defun rdindex-describe-function-2 ()
  "Search the RD documentation of FUNC. ruby-info.el compatible IF."
  (interactive)
  (rdindex-describe-function (rdindex-read-function-2)))

(defun rdindex-describe-function-briefly-2 ()
  "Search the RD documentation of FUNC. ruby-info.el compatible IF."
  (interactive)
  (let ((buf (get-buffer-create " *rdindex*tmp"))
        (func (rdindex-read-function-2)))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (call-process rdindex-command nil buf nil "-usage" func)
      (message "%s" (buffer-substring (point-min)(point-max))))))


(defun rdindex-read-function (prompt table)
  (if rdindex-completion-table
      (let (( completion-ignore-case t))
        (completing-read prompt table))
    (rdindex-make-completion-table)
    (rdindex-read-function prompt table)))

;;Thanx Masatoshi TSUCHIYA <tsuchiya@pine.kuee.kyoto-u.ac.jp>
(defun rdindex-completing-read (&rest args)
  (let ((original-map (copy-keymap minibuffer-local-must-match-map)))
    (unwind-protect
        (progn
          (define-key minibuffer-local-must-match-map "?" nil)
          (apply 'completing-read args))
      (setq minibuffer-local-must-match-map original-map))))
  
(defun rdindex-read-string-with-default ()
  "completing-read with default value for ruby."
  (let* (( case-fold-search nil)
         (cw (current-word))
         (v (save-excursion
              (while (and
                      (not (assoc cw rdindex-method-table))
                      (forward-word -1))
                (setq cw (current-word)))
              (if (assoc cw rdindex-method-table)
                  cw
                nil)))
         (var (rdindex-completing-read (if v (format "Show rdindex (default %s): " v)
                                 (format "Show rdindex: "))
                               rdindex-method-table nil t)))
    (if (equal var "") v var)))


(defun rdindex-get-line ()
  (save-excursion
    (let ((b (progn
               (beginning-of-line)
               (point)))
          (e (progn
               (end-of-line)
               (point))))
      (buffer-substring b e))))
      

(defun rdindex-make-completion-table ()
  "Make rdindex-completion-table."
  (interactive)
  (let ((buf (get-buffer-create " *rdindex*tmp")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (call-process rdindex-command nil buf nil "-make_index_el")
      (eval-buffer))))
      
(defun rdindex-remake-index (arg)
  "Remake rdindex-personal.dat and completion table."
  (interactive "p")
  (save-some-buffers nil nil)
  (if (= arg 16)
      (call-process rdindex-command nil nil nil "-make_index"))
  (call-process rdindex-command nil nil nil
                (if (< 1 arg)
                    "-make_personal_index"
                  "-update_personal_index") (buffer-file-name))
  (rdindex-make-completion-table))

(defun rdindex-search-method (func)
  "Return (filename lineno) of the RD documentation of FUNC."
  (let ((buf (get-buffer-create " *rdindex*tmp")) p1 p2 filename lineno)
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (call-process rdindex-command nil buf nil func)
      (goto-char (point-min))
      (re-search-forward "^\\(.+\\) \\(.+\\) \\(.+\\)$" nil t)
      (setq filename (buffer-substring (match-beginning 2)(match-end 2))
            lineno (string-to-int (buffer-substring (match-beginning 3) (match-end 3))))
      (cons filename lineno)
      )))

(defun rdindex-display-buffer (buf)
  "Default display function."
  (switch-to-buffer-other-window buf)
  (recenter 0)
  (other-window -1)
  )

(defun ruby-find-completion-default ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_\\|:")
      (forward-char 1))
    (if (or (re-search-backward "\\sw\\|\\s_\\|:"
				(save-excursion (beginning-of-line) (point))
				t)
	    (re-search-forward "\\(\\sw\\|\\s_\\|:\\)+"
			       (save-excursion (end-of-line) (point))
			       t))
	(progn (goto-char (match-end 0))
	       (buffer-substring (point)
				 (progn (forward-sexp -1)
					(while (looking-at "\\s'")
					  (forward-char 1))
					(point))))
      nil)))

(defun ruby-complete-method ()
  "Perform ruby method and class completion on the text around point."
  (interactive)
  (or rdindex-method-table
      (error "%s"
	     (substitute-command-keys
	      "No completion table loaded; try \\[rdindex-remake-index]")))
  (let ((pattern (ruby-find-completion-default))
        (method-with-class-func
         (lambda (method)
           (concat
            method "  ["
            (mapconcat 'car (cdr (assoc method rdindex-method-klass-hash)) ", ")
            "]")))
	beg
	completion)
    (or pattern
	(error "Nothing to complete"))
    (search-backward pattern)
    (setq beg (point))
    (forward-char (length pattern))
    (setq completion (try-completion pattern rdindex-method-klass-table nil))
    (cond ((eq completion t)
           (message "%s" (funcall method-with-class-func pattern)))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg (point))
	   (insert completion)
           (message "%s" (funcall method-with-class-func completion)))
	  (t
	   (message "Making completion list...")
	   (with-output-to-temp-buffer "*Completions*"
	     (display-completion-list
	      (mapcar method-with-class-func
                      (all-completions pattern rdindex-method-klass-table nil))))
	   (message "Making completion list...%s" "done")))))

;;; match-string
(or (fboundp 'match-string)
    ;; Introduced in Emacs 19.29.
    (defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num))))))

(provide 'rdindex)