;;; -*- Emacs-Lisp -*-
;;; $Id: sdic-sgml.el,v 2.2 1999/01/27 14:02:17 tsuchiya Exp $

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of sdic. Please see sdic.el for more detail.

;; SDIC �����μ���򰷤�����Υ饤�֥��Ǥ���


;;; Install:

;; (1) �����Ŭ�ڤʷ������Ѵ����ơ�Ŭ���ʾ��( ��: /usr/dict/ )����¸
;;     ���Ʋ������������Ѵ��ѥ�����ץȤȤ��ưʲ��� Perl ������ץȤ�
;;     ���ѤǤ��ޤ���
;;
;;         gene.perl    - GENE95 ����
;;         edict.perl   - EDICT ����
;;         eijirou.perl - �Ѽ�Ϻ
;;
;; (2) �Ȥ���褦�ˤ���������������� sdic-eiwa-dictionary-list �ޤ�
;;     �� sdic-waei-dictionary-list ���ɲä��Ʋ�������
;;
;;         (setq sdic-eiwa-dictionary-list
;;               (cons '(sdic-sgml "/usr/dict/gene.dic") sdic-eiwa-dictionary-list))
;;
;;     �����������ϼ��Τ褦�ʹ����ˤʤäƤ��ޤ���
;;
;;         (sdic-sgml �ե�����̾ (���ץ����A ��A) (���ץ����B ��B) ...)
;;
;;     ���̤ʻ��꤬���פʾ��ˤϡ����ץ����Ͼ�ά�Ǥ��ޤ���
;;
;;         (sdic-sgml �ե�����̾)


;;; Options:

;; sdic-sgml.el ���Ф��ƻ���Ǥ��륪�ץ����ϼ����̤�Ǥ���
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
;; extract
;;     ���̼����Ÿ�����뤿��γ������ޥ�ɤ���ꤷ�ޤ�����ά�������
;;     �ϡ����񤬰��̤���Ƥ��ʤ��ȸ��ʤ��ޤ���
;;
;; extract-option
;;     extract ���ץ����ˤ�äƻ��ꤵ�줿�������ޥ�ɤ��Ф��ơ�����
;;     ��Ÿ������ɸ����Ϥ˽��Ϥ����뤿��Υ��ޥ�ɥ饤���������ꤷ
;;     �ޤ�����ά�������� sdic-sgml-extract-option ���ͤ�Ȥ��ޤ���


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
(provide 'sdic-sgml)
(put 'sdic-sgml 'version "2.0")
(put 'sdic-sgml 'init-dictionary 'sdic-sgml-init-dictionary)
(put 'sdic-sgml 'open-dictionary 'sdic-sgml-open-dictionary)
(put 'sdic-sgml 'close-dictionary 'sdic-sgml-close-dictionary)
(put 'sdic-sgml 'search-entry 'sdic-sgml-search-entry)
(put 'sdic-sgml 'get-content 'sdic-sgml-get-content)


;;;----------------------------------------------------------------------
;;;		���/�ѿ������
;;;----------------------------------------------------------------------

(defvar sdic-sgml-extract-option "-dc" "\
*Option for archiver.
���̼����Ÿ�����뤿��˻Ȥ����ץ����")

(defconst sdic-sgml-buffer-name " *sdic-sgml*")



;;;----------------------------------------------------------------------
;;;		SDIC�����򰷤�����Υ饤�֥��
;;;----------------------------------------------------------------------

(defsubst sdic-sgml-replace-string (string from to) "\
ʸ���� STRING �˴ޤޤ�Ƥ���ʸ���� FROM ������ʸ���� TO ���ִ�����ʸ������֤�
FROM �ˤ�����ɽ����ޤ�ʸ��������Ǥ��뤬��TO �ϸ���ʸ���󤷤������
���ʤ��Τǡ���դ��ƻȤ����ȡ�"
  (let ((start 0) list)
    (while (string-match from string start)
      (setq list (cons to (cons (substring string start (match-beginning 0)) list))
	    start (match-end 0)))
    (eval (cons 'concat (nreverse (cons (substring string start) list))))))


(defsubst sdic-sgml-recover-string (str &optional recover-lf)
  "STR �˴ޤޤ�Ƥ��륨��������ʸ�������������"
  (save-match-data
    (setq str (sdic-sgml-replace-string str "&lt;" "<"))
    (setq str (sdic-sgml-replace-string str "&gt;" ">"))
    (if recover-lf
	(setq str (sdic-sgml-replace-string str "&lf;" "\n")))
    (sdic-sgml-replace-string str "&amp;" "&")))


(defun sdic-sgml-recover-region (start end &optional recover-lf)
  "�꡼�����˴ޤޤ�Ƥ��륨��������ʸ�������������"
  (save-excursion
    (save-match-data
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(while (search-forward "&lt;" nil t)
	  (replace-match "<" t t))
	(goto-char (point-min))
	(while (search-forward "&gt;" nil t)
	  (replace-match ">" t t))
	(if recover-lf
	    (progn
	      (goto-char (point-min))
	      (while (search-forward "&lf;" nil t)
		(replace-match "\n" t t))))
	(goto-char (point-min))
	(while (search-forward "&amp;" nil t)
	  (replace-match "&" t t))
	))))


(defsubst sdic-sgml-escape-string (str &optional escape-lf)
  "STR �˴ޤޤ�Ƥ����ü�ʸ���򥨥������פ���"
  (save-match-data
    (setq str (sdic-sgml-replace-string str "&" "&amp;"))
    (if escape-lf
	(setq str (sdic-sgml-replace-string str "\n" "&lf;")))
    (setq str (sdic-sgml-replace-string str "<" "&lt;"))
    (sdic-sgml-replace-string str ">" "&gt;")))


(defun sdic-sgml-escape-region (start end &optional escape-lf)
  "�꡼�����˴ޤޤ�Ƥ����ü�ʸ���򥨥������פ���"
  (save-excursion
    (save-match-data
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(while (search-forward "&" nil t)
	  (replace-match "&amp;" t t))
	(goto-char (point-min))
	(while (search-forward "<" nil t)
	  (replace-match "&lt;" t t))
	(goto-char (point-min))
	(while (search-forward ">" nil t)
	  (replace-match "&gt;" t t))
	(if escape-lf
	    (progn
	      (goto-char (point-min))
	      (while (search-forward "\n" nil t)
		(replace-match "&lf;" t t))))
	))))


(defsubst sdic-sgml-make-query-string (string search-type)
  "STR ����Ŭ�ڤʸ���ʸ�������������"
  (cond
   ;; �������׸����ξ��
   ((eq search-type nil) (concat "<K>" (sdic-sgml-escape-string (downcase string))))
   ;; �������׸����ξ��
   ((eq search-type t) (concat (sdic-sgml-escape-string (downcase string)) "</K>"))
   ;; �������׸����ξ��
   ((eq search-type 'lambda) (concat "<K>" (sdic-sgml-escape-string (downcase string)) "</K>"))
   ;; ��ʸ�����ξ��
   ((eq search-type 0) (sdic-sgml-escape-string string))
   ;; ����ʳ��θ�����������ꤵ�줿���
   (t (error "Not supported search type is specified. \(%s\)"
	     (prin1-to-string search-type)))))


(defsubst sdic-sgml-get-entry (&optional add-keys-to-headword) "\
���߹Ԥ��鸫�Ф������Ф�������ʸ����Ƭ�ΰ��֤���롣
point ����Ƭ�ˤ�����֤ǸƤӽФ��ʤ���Фʤ�ʤ���"
  (save-excursion
    (save-match-data
      (if (looking-at "<")
	  (let ((top (goto-char (+ 3 (point))))
		(end (progn (search-forward "<") (match-beginning 0)))
		(pos (progn (end-of-line) (search-backward ">") (match-end 0))))
	    (cons (sdic-sgml-recover-string
		   (if (and add-keys-to-headword (> (- pos end) 11))
		       (format "%s [%s]"
			       (buffer-substring top end)
			       (sdic-sgml-replace-string (buffer-substring (+ end 7) (- pos 4))
							 "</K><K>" "]["))
		     (buffer-substring top end)))
		  pos))))))



;;;----------------------------------------------------------------------
;;;		����
;;;----------------------------------------------------------------------

(defun sdic-sgml-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (sdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "sdic-sgml+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (if (get dic 'extract)
	      (or (get dic 'extract-option)
		  (put dic 'extract-option sdic-sgml-extract-option)))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system sdic-default-coding-system))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun sdic-sgml-open-dictionary (dic)
  "Function to open dictionary"
  (if (or (sdic-buffer-live-p (get dic 'sdic-sgml-buffer))
	  (save-excursion
	    (set-buffer (put dic 'sdic-sgml-buffer (generate-new-buffer sdic-sgml-buffer-name)))
	    (prog1 (if (get dic 'extract)
		       (= 0 (sdic-call-process (get dic 'extract) nil t nil
					       (get dic 'coding-system)
					       (get dic 'extract-option)
					       (get dic 'file-name)))
		     (condition-case err
			 (sdic-insert-file-contents (get dic 'file-name) (get dic 'coding-system))
		       (error nil)))
	      (insert "\n")
	      (setq buffer-read-only t)
	      (set-buffer-modified-p nil))))
      dic))


(defun sdic-sgml-close-dictionary (dic)
  "Function to close dictionary"
  (kill-buffer (get dic 'sdic-sgml-buffer))
  (put dic 'sdic-sgml-buffer nil))


(defun sdic-sgml-search-entry (dic string &optional search-type) "\
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
    (set-buffer (get dic 'sdic-sgml-buffer))
    (goto-char (point-min))
    (setq string (sdic-sgml-make-query-string string search-type))
    (let ((case-fold-search nil)
	  (add-keys (get dic 'add-keys-to-headword))
	  ret)
      (while (if (search-forward string nil t)
		 (progn
		   (beginning-of-line)
		   (setq ret (cons (sdic-sgml-get-entry add-keys) ret))
		   (= 0 (forward-line)))))
      (nreverse (delq nil ret)))))


(defun sdic-sgml-get-content (dic point)
  (save-excursion
    (set-buffer (get dic 'sdic-sgml-buffer))
    (if (<= point (point-max))
	(sdic-sgml-recover-string (buffer-substring (goto-char point)
						    (progn (end-of-line) (point))))
      (error "Can't find content. (ID=%d)" point))))
