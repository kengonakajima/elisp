;;; -*- Emacs-Lisp -*-
;;; $Id: sdic-gene.el,v 2.1 1999/01/27 14:22:39 tsuchiya Exp $

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of sdic. Please see sdic.el for more detail.

;; COMPAT 形式の辞書を外部プログラムに頼らずに検索するライブラリです。
;; COMPAT 形式の詳細については sdic.texi を参照して下さい。


;;; Install:

;; (1) 辞書を適切な形式に変換して、適当な場所( 例: /usr/dict/ )に保存
;;     して下さい。辞書変換用スクリプトとして以下の Perl スクリプトが
;;     利用できます。
;;
;;         gene.perl    - GENE95 辞書
;;         jgene.perl   - GENE95 辞書から和英辞書を生成する
;;         eijirou.perl - 英辞郎
;;
;; (2) 使えるようにした辞書の定義情報を sdic-eiwa-dictionary-list また
;;     は sdic-waei-dictionary-list に追加して下さい。
;;
;;         (setq sdic-eiwa-dictionary-list
;;               (cons '(sdic-gene "/usr/dict/gene.dic") sdic-eiwa-dictionary-list))
;;
;;     辞書定義情報は次のような構成になっています。
;;
;;         (sdic-gene ファイル名 (オプションA 値A) (オプションB 値B) ...)
;;
;;     特別な指定が不要な場合には、オプションは省略できます。
;;
;;         (sdic-gene ファイル名)


;;; Options:

;; sdic-gene.el に対して指定できるオプションは次の通りです。
;;
;; coding-system
;;     辞書の漢字コードを指定します。省略した場合は、
;;     sdic-default-coding-system の値を使います。
;;
;; title
;;     辞書のタイトルを指定します。省略した場合は、辞書ファイルの 
;;     basename をタイトルとします。
;;
;; extract
;;     圧縮辞書を展開するための外部コマンドを指定します。省略した場合
;;     は、辞書が圧縮されていないと見なします。
;;
;; extract-option
;;     extract オプションによって指定された外部コマンドに対して、辞書
;;     を展開して標準出力に出力させるためのコマンドライン引数を指定し
;;     ます。省略した場合は sdic-gene-extract-option の値を使います。


;;; Note:

;; sdic-compat.el と sdic-gene.el は同じ機能を提供しているライブラリで
;; す。sdic-compat.el は外部コマンドを呼び出しているのに対して、
;; sdic-gene.el は Emacs の機能のみを利用しています。ただし、辞書をバッ
;; ファに読み込んでから検索を行なうので、大量のメモリが必要になります。
;;
;; Default の設定では、必要な外部コマンドが見つかった場合は 
;; sdic-compat.el を、見つからなかった場合には sdic-gene.el を使うよう
;; になっています。


;;; ライブラリ定義情報
(require 'sdic)
(provide 'sdic-gene)
(put 'sdic-gene 'version "2.0")
(put 'sdic-gene 'init-dictionary 'sdic-gene-init-dictionary)
(put 'sdic-gene 'open-dictionary 'sdic-gene-open-dictionary)
(put 'sdic-gene 'close-dictionary 'sdic-gene-close-dictionary)
(put 'sdic-gene 'search-entry 'sdic-gene-search-entry)
(put 'sdic-gene 'get-content 'sdic-gene-get-content)


;;;----------------------------------------------------------------------
;;;		定数/変数の宣言
;;;----------------------------------------------------------------------

(defvar sdic-gene-extract-option "-dc" "\
*Option for archiver.
圧縮辞書を展開するために使うオプション")

(defconst sdic-gene-search-buffer-name " *sdic-gene*")



;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

(defun sdic-gene-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (sdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "sdic-gene+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (if (get dic 'extract)
	      (or (get dic 'extract-option)
		  (put dic 'extract-option sdic-gene-extract-option)))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system sdic-default-coding-system))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun sdic-gene-open-dictionary (dic)
  "Function to open dictionary"
  (if (or (sdic-buffer-live-p (get dic 'sdic-gene-search-buffer))
	  (save-excursion
	    (set-buffer (put dic 'sdic-gene-search-buffer (generate-new-buffer sdic-gene-search-buffer-name)))
	    (insert "\n")
	    (prog1 (if (get dic 'extract)
		       (= 0 (sdic-call-process (get dic 'extract) nil t nil
					       (get dic 'coding-system)
					       (get dic 'extract-option)
					       (get dic 'file-name)))
		     (condition-case err
			 (sdic-insert-file-contents (get dic 'file-name) (get dic 'coding-system))
		       (error nil)))
	      (setq buffer-read-only t)
	      (set-buffer-modified-p nil))))
      dic))


(defun sdic-gene-close-dictionary (dic)
  "Function to close dictionary"
  (kill-buffer (get dic 'sdic-gene-search-buffer))
  (put dic 'sdic-gene-search-buffer nil))


(defsubst sdic-gene-search-internal (string)
  "通常の検索を行う内部関数"
  (let (ret (case-fold-search t))
    (while (search-forward string nil t)
      (save-excursion
	(setq ret (cons (cons (buffer-substring (progn (beginning-of-line) (point))
						(progn (skip-chars-forward "^\t") (point)))
			      (1+ (point)))
			ret))))
    (nreverse ret)))


(defsubst sdic-gene-re-search-internal (string)
  "正規表現検索を行う内部関数"
  (let (ret (case-fold-search t))
    (while (re-search-forward string nil t)
      (save-excursion
	(setq ret (cons (cons (buffer-substring (progn (beginning-of-line) (point))
						(progn (skip-chars-forward "^\t") (point)))
			      (1+ (point)))
			ret))))
    (nreverse ret)))


(defun sdic-gene-search-entry (dic string &optional search-type) "\
Function to search word with look or grep, and write results to current buffer.
search-type の値によって次のように動作を変更する。
    nil    : 前方一致検索
    t      : 後方一致検索
    lambda : 完全一致検索
    0      : 全文検索
    regexp : 正規表現検索
検索結果として見つかった見出し語をキーとし、その定義文の先頭の point を値とする
連想配列を返す。
"
  (save-excursion
    (set-buffer (get dic 'sdic-gene-search-buffer))
    (goto-char (point-min))
    (cond
     ;; 前方一致検索
     ((eq search-type nil)
      (sdic-gene-search-internal (concat "\n" string)))
     ;; 後方一致検索
     ((eq search-type t)
      (sdic-gene-search-internal (concat string "\t")))
     ;; 完全一致検索
     ((eq search-type 'lambda)
      (sdic-gene-search-internal (concat "\n" string "\t")))
     ;; 全文検索
     ((eq search-type 0)
      (sdic-gene-search-internal string))
     ;; 正規表現検索
     ((eq search-type 'regexp)
      (sdic-gene-re-search-internal string))
     ;; それ以外の検索形式を指定された場合
     (t (error "Not supported search type is specified. \(%s\)"
	       (prin1-to-string search-type))))))


(defun sdic-gene-get-content (dic point)
  (save-excursion
    (set-buffer (get dic 'sdic-gene-search-buffer))
    (if (<= point (point-max))
	(buffer-substring (goto-char point) (progn (end-of-line) (point)))
      (error "Can't find content. (ID=%d)" point))))
