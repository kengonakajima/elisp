;;; -*- Emacs-Lisp -*-
;;; $Id: sdic-grep.el,v 2.2 1999/01/27 14:11:12 tsuchiya Exp $

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of sdic. Please see sdic.el for more detail.

;; SDIC �����μ���� grep �����Ѥ��Ƹ�������饤�֥��Ǥ���


;;; Install:

;; (1) ����򸡺����뤿��� grep �����Ѥ��Ƥ��ޤ����ѥ����̤äƤ��뤫
;;     ��ǧ���Ʋ�������
;;
;; (2) �����Ŭ�ڤʷ������Ѵ����ơ�Ŭ���ʾ��( ��: /usr/dict/ )����¸
;;     ���Ʋ������������Ѵ��ѥ�����ץȤȤ��ưʲ��� Perl ������ץȤ�
;;     ���ѤǤ��ޤ���
;;
;;         gene.perl    - GENE95 ����
;;         edict.perl   - EDICT ����
;;         eijirou.perl - �Ѽ�Ϻ
;;
;; (3) �Ȥ���褦�ˤ���������������� sdic-eiwa-dictionary-list �ޤ�
;;     �� sdic-waei-dictionary-list ���ɲä��Ʋ�������
;;
;;         (setq sdic-eiwa-dictionary-list
;;               (cons '(sdic-grep "/usr/dict/gene.dic") sdic-eiwa-dictionary-list))
;;
;;     �����������ϼ��Τ褦�ʹ����ˤʤäƤ��ޤ���
;;
;;         (sdic-grep �ե�����̾ (���ץ����A ��A) (���ץ����B ��B) ...)
;;
;;     ���̤ʻ��꤬���פʾ��ˤϡ����ץ����Ͼ�ά�Ǥ��ޤ���
;;
;;         (sdic-grep �ե�����̾)


;;; Options:

;; sdic-grep.el ���Ф��ƻ���Ǥ��륪�ץ����ϼ����̤�Ǥ���
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
;;     sdic-grep-command ���ͤ�Ȥ��ޤ���


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
(provide 'sdic-grep)
(put 'sdic-grep 'version "2.0")
(put 'sdic-grep 'init-dictionary 'sdic-grep-init-dictionary)
(put 'sdic-grep 'open-dictionary 'sdic-grep-open-dictionary)
(put 'sdic-grep 'close-dictionary 'sdic-sgml-close-dictionary)
(put 'sdic-grep 'search-entry 'sdic-grep-search-entry)
(put 'sdic-grep 'get-content 'sdic-grep-get-content)


;;;----------------------------------------------------------------------
;;;		���/�ѿ������
;;;----------------------------------------------------------------------

(defvar sdic-grep-command
  (catch 'which
    (mapcar '(lambda (file)
	       (mapcar '(lambda (path)
			  (if (file-executable-p (expand-file-name file path))
			      (throw 'which (expand-file-name file path))))
		       exec-path))
	    '("fgrep" "fgrep.exe" "grep" "grep.exe")))
  "*Executable file name of grep")

(defconst sdic-grep-buffer-name " *sdic-grep*")



;;;----------------------------------------------------------------------
;;;		����
;;;----------------------------------------------------------------------

(defun sdic-grep-available-p ()
  (stringp sdic-grep-command))


(defun sdic-grep-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (sdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "sdic-grep+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (or (get dic 'command)
	      (put dic 'command sdic-grep-command))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system sdic-default-coding-system))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun sdic-grep-open-dictionary (dic)
  "Function to open dictionary"
  (and (or (sdic-buffer-live-p (get dic 'sdic-sgml-buffer))
	   (put dic 'sdic-sgml-buffer (generate-new-buffer sdic-grep-buffer-name)))
       dic))


(defun sdic-grep-search-entry (dic string &optional search-type) "\
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
    (let ((add-keys (get dic 'add-keys-to-headword))
	  limit ret)
      (if (get dic 'sdic-grep-erase-buffer)
	  (delete-region (point-min) (point-max)))
      (setq limit (goto-char (point-max)))
      (put dic 'sdic-grep-erase-buffer nil)
      (sdic-call-process (get dic 'command) nil t nil
			 (get dic 'coding-system)
			 (sdic-sgml-make-query-string string search-type)
			 (get dic 'file-name))
      ;; �Ƹ�����̤� ID ����Ϳ����
      (goto-char limit)
      (while (progn
	       (setq ret (cons (sdic-sgml-get-entry add-keys) ret))
	       (= 0 (forward-line 1))))
      (nreverse (delq nil ret)))))


(defun sdic-grep-get-content (dic point)
  (put dic 'sdic-grep-erase-buffer t)
  (sdic-sgml-get-content dic point))
