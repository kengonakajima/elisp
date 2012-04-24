;;; -*- Emacs-Lisp -*-
;;; $Id: sdic-array.el,v 2.3 1999/01/27 14:06:36 tsuchiya Exp $

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of sdic. Please see sdic.el for more detail.

;; SDIC �����μ���� array �����Ѥ��Ƹ�������饤�֥��Ǥ���


;;; Install:

;; (1) array �Ȥϡ�SUFARY ����°���Ƥ������÷������ץ����Ǥ���
;;     SUFARY �ˤĤ��Ƥϰʲ��� URL �򻲾Ȥ��Ʋ�������
;;
;;         http://cactus.aist-nara.ac.jp/lab/nlt/ss/
;;
;;     �����оݤΥƥ����Ȥκ���������˺������Ƥ��������פθ����ץ�
;;     ���ʤΤǡ�grep �����®�ʸ�������ǽ�Ǥ���
;;
;;     ��°ʸ��λؼ��ˤ������äơ�array �� mkary �򥤥󥹥ȡ��뤷�Ʋ�
;;     ������
;;
;; (2) �����Ŭ�ڤʷ������Ѵ����ơ�Ŭ���ʾ��( ��: /usr/dict/ )����¸
;;     ���Ʋ������������Ѵ��ѥ�����ץȤȤ��ưʲ��� Perl ������ץȤ�
;;     ���ѤǤ��ޤ���
;;
;;         gene.perl    - GENE95 ����
;;         edict.perl   - EDICT ����
;;         eijirou.perl - �Ѽ�Ϻ
;;
;; (3) ����κ������������ޤ���/usr/dict/gene.dic �κ���������������
;;     �ϡ����Τ褦�˥��ޥ�ɤ����Ϥ��Ʋ�������
;;
;;         mkary /usr/dict/gene.dic
;;
;;     ����ȡ�/usr/dict/gene.dic.ary ����������ޤ���
;;
;;
;; (4) �Ȥ���褦�ˤ���������������� sdic-eiwa-dictionary-list �ޤ�
;;     �� sdic-waei-dictionary-list ���ɲä��Ʋ�������
;;
;;         (setq sdic-eiwa-dictionary-list
;;               (cons '(sdic-array "/usr/dict/gene.dic") sdic-eiwa-dictionary-list))
;;
;;     �����������ϼ��Τ褦�ʹ����ˤʤäƤ��ޤ���
;;
;;         (sdic-array �ե�����̾ (���ץ����A ��A) (���ץ����B ��B) ...)
;;
;;     ���̤ʻ��꤬���פʾ��ˤϡ����ץ����Ͼ�ά�Ǥ��ޤ���
;;
;;         (sdic-array �ե�����̾)


;;; Options:

;; sdic-array.el ���Ф��ƻ���Ǥ��륪�ץ����ϼ����̤�Ǥ���
;;
;; coding-system
;;     ����δ��������ɤ���ꤷ�ޤ�����ά�������ϡ�
;;     sdic-default-coding-system ���ͤ�Ȥ��ޤ���
;;
;; title
;;     ����Υ����ȥ����ꤷ�ޤ�����ά�������ϡ�����ե������ 
;;     basename �򥿥��ȥ�Ȥ��ޤ���
;;
;; add-keys-to-headword
;;     ���Ƥθ���������ޤ�Ƹ��Ф������������� t �����ꤷ�Ʋ���
;;     �����±Ѽ���򸡺�������ˡ����겾̾��ޤ�ƽ��Ϥ��������
;;     �Ѥ��ޤ���
;;
;; command
;;     �������ޥ�ɤ�̾������ꤷ�ޤ�����ά�������� 
;;     sdic-array-command ���ͤ�Ȥ��ޤ���
;;
;; array-file-name
;;     ����� array file ��̾������ꤷ�ޤ�����ά�������ϡ���������
;;     ��ɤΥǥե�����ͤ��Ȥ��ޤ���


;;; Note;

;; sdic-sgml.el , sdic-grep.el , sdic-array.el �� SDIC �����μ����
;; �����뤿��Υ饤�֥��Ǥ������줾��ΰ㤤�ϼ����̤�Ǥ���
;;
;; ��sdic-sgml.el
;;     ����ǡ��������ƥ�����ɤ߹���Ǥ��鸡����Ԥ��ޤ�����������
;;     ��ɤ�ɬ�פȤ��ޤ��󤬡����̤Υ��꤬ɬ�פˤʤ�ޤ���
;;
;; ��sdic-grep.el
;;     grep �����Ѥ��Ƹ�����Ԥ��ޤ���
;;
;; ��sdic-array.el
;;     array �����Ѥ��Ƹ�����Ԥ��ޤ�������� index file �����������
;;     ���Ƥ����Ƥ��鸡����Ԥ��ޤ��Τǡ���®�˸�������ǽ�Ǥ�����������
;;     index file �ϼ����3�����٤��礭���ˤʤ�ޤ���
;;
;; ���Ū�����Ϥμ���򸡺�������� sdic-grep.el ����Ŭ�Ǥ��礦����
;; ������5MByte ����礭������ξ��� sdic-array.el �����Ѥ��θ����
;; �����Ȼפ��ޤ���
;;
;; SDIC �����μ���ι�¤�ˤĤ��Ƥϡ�sdic.texi �򻲾Ȥ��Ƥ���������


;;; �饤�֥���������
(require 'sdic)
(require 'sdic-sgml)
(provide 'sdic-array)
(put 'sdic-array 'version "2.0")
(put 'sdic-array 'init-dictionary 'sdic-array-init-dictionary)
(put 'sdic-array 'open-dictionary 'sdic-array-open-dictionary)
(put 'sdic-array 'close-dictionary 'sdic-array-close-dictionary)
(put 'sdic-array 'search-entry 'sdic-array-search-entry)
(put 'sdic-array 'get-content 'sdic-array-get-content)


;;;----------------------------------------------------------------------
;;;		���/�ѿ������
;;;----------------------------------------------------------------------

(defvar sdic-array-command
  (catch 'which
    (mapcar '(lambda (file)
	       (mapcar '(lambda (path)
			  (if (file-executable-p (expand-file-name file path))
			      (throw 'which (expand-file-name file path))))
		       exec-path))
	    '("array" "array.exe")))
  "*Executable file name of array")

(defvar sdic-array-wait-prompt-flag nil)

(defconst sdic-array-buffer-name " *sdic-array*")



;;;----------------------------------------------------------------------
;;;		����
;;;----------------------------------------------------------------------

(defun sdic-array-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (sdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "sdic-array+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (or (get dic 'command)
	      (put dic 'command sdic-array-command))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system sdic-default-coding-system))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun sdic-array-open-dictionary (dic)
  "Function to open dictionary"
  (let	((old-buffer (current-buffer))
	 (buf (or (sdic-buffer-live-p (get dic 'sdic-sgml-buffer))
		  (put dic 'sdic-sgml-buffer (generate-new-buffer sdic-array-buffer-name)))))
    (unwind-protect
	(and (or (sdic-array-process-live-p dic)
		 (let ((limit (progn (set-buffer buf) (goto-char (point-max))))
		       (proc (sdic-start-process "array" buf
						 (get dic 'coding-system)
						 (get dic 'command)
						 (get dic 'file-name)
						 (or (get dic 'array-file-name) ""))))
		   (accept-process-output proc 5)
		   (if (search-backward "ok\n" limit t)
		       (progn
			 (set-process-filter proc 'sdic-array-wait-prompt)
			 (process-kill-without-query proc)
			 (sdic-array-send-string proc "style line")
			 t))))
	     dic)
      (set-buffer old-buffer))))


(defun sdic-array-close-dictionary (dic)
  "Function to close dictionary"
  (let ((proc (sdic-array-process-live-p dic)))
    (if proc
	(progn
	  (set-process-filter proc nil)
	  (process-send-string proc "quit\n"))))
  (kill-buffer (get dic 'sdic-sgml-buffer))
  (put dic 'sdic-sgml-buffer nil))


(defun sdic-array-search-entry (dic string &optional search-type) "\
Function to search word with look or grep, and write results to current buffer.
search-type ���ͤˤ�äƼ��Τ褦��ư����ѹ����롣
    nil    : �������׸���
    t      : �������׸���
    lambda : �������׸���
    0      : ��ʸ����
������̤Ȥ��Ƹ��Ĥ��ä����Ф���򥭡��Ȥ����������ʸ����Ƭ�� point ���ͤȤ���
Ϣ��������֤���
"
  (save-excursion
    (let* ((buf (set-buffer (get dic 'sdic-sgml-buffer)))
	   (proc (get-buffer-process buf))
	   (add-keys (get dic 'add-keys-to-headword))
	   pos ret cons end lst)
      (if (get dic 'sdic-array-erase-buffer)
	  (delete-region (point-min) (point-max))
	(goto-char (point-max)))
      (put dic 'sdic-array-erase-buffer nil)
      (sdic-array-send-string proc "init") ; ������������
      (setq pos (point))
      (sdic-array-send-string proc (format "search %s" (sdic-sgml-make-query-string string search-type)))
      (if (re-search-backward "^FOUND: [0-9]+$" pos t)
	  (progn
	    (setq pos (+ 3 (match-end 0)))
	    (sdic-array-send-string proc "show")
	    ;; �Ƹ�����̤� ID ����Ϳ����
	    (goto-char pos)
	    (while (progn
		     (setq cons (sdic-sgml-get-entry add-keys)
			   end (progn (end-of-line) (point)))
		     (if cons
			 (or (and (setq lst (assoc (car cons) ret))
				  (= 0 (compare-buffer-substrings buf pos end
								  buf (nth 2 lst) (nth 3 lst))))
			     (setq ret (cons (list (car cons) (cdr cons) pos end) ret))))
		     (if (eobp) nil (goto-char (1+ (point)))))
	      (setq pos (point)))
	    (mapcar (function (lambda (l) (cons (car l) (nth 1 l)))) (nreverse ret)))))))


(defun sdic-array-get-content (dic point)
  (put dic 'sdic-array-erase-buffer t)
  (sdic-sgml-get-content dic point))


(defun sdic-array-process-live-p (dic)
  (let ((proc (get-buffer-process (get dic 'sdic-sgml-buffer))))
    (and (processp proc)
	 (eq (process-status proc) 'run)
	 proc)))


(defun sdic-array-send-string (proc string) "\
Send STRING as command to process."
  (setq string (concat string "\n"))
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let ((sdic-array-wait-prompt-flag t))
	  (set-buffer (process-buffer proc))
	  (goto-char (point-max))
	  (insert string)
	  (set-marker (process-mark proc) (point))
	  (process-send-string proc string)
	  (while sdic-array-wait-prompt-flag (accept-process-output proc)))
      (set-buffer old-buffer))))


(defun sdic-array-wait-prompt (proc string) "\
Process filter function of Array.
�ץ��ץȤ����줿���Ȥ��Τ��ơ�sdic-array-wait-prompt-flag �� nil 
�ˤ��롣"
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(save-match-data ; Emacs-19.34 �ʹߤϼ�ưŪ�˸�����̤�����/�������Ԥ���Τ�����
	  (set-buffer (process-buffer proc))
	  (let ((start (point)))
	    (goto-char (process-mark proc))
	    (insert string)
	    (set-marker (process-mark proc) (point))
	    (skip-chars-backward " \t\n")
	    (beginning-of-line)
	    (if (looking-at "ok\n")
		(progn
		  (goto-char (match-end 0))
		  (setq sdic-array-wait-prompt-flag nil))
	      (goto-char start))))
      (set-buffer old-buffer))))
