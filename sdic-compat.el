;;; -*- Emacs-Lisp -*-
;;; $Id: sdic-compat.el,v 2.1 1999/01/27 14:18:19 tsuchiya Exp $

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of sdic. Please see sdic.el for more detail.

;; COMPAT 形式の辞書を外部プログラム( look / grep )を利用して検索する
;; ライブラリです。COMPAT 形式の詳細については sdic.texi を参照して下
;; さい。


;;; Install:

;; (1) look と大文字/小文字の違いを無視した検索が出来る grep ( fgrep 
;;     または GNU grep )が必要です。また、正規表現検索を利用する場合は 
;;     egrep も必要です。パスが通っているか確認して下さい。
;;
;; (2) 辞書を適切な形式に変換して、適当な場所( 例: /usr/dict/ )に保存
;;     して下さい。辞書変換用スクリプトとして以下の Perl スクリプトが
;;     利用できます。
;;
;;         gene.perl    - GENE95 辞書
;;         jgene.perl   - GENE95 辞書から和英辞書を生成する
;;         eijirou.perl - 英辞郎
;;
;;     --compat オプションを指定する必要があります。
;;
;; (3) 使えるようにした辞書の定義情報を sdic-eiwa-dictionary-list また
;;     は sdic-waei-dictionary-list に追加して下さい。
;;
;;         (setq sdic-eiwa-dictionary-list
;;               (cons '(sdic-compat "/usr/dict/gene.dic") sdic-eiwa-dictionary-list))
;;
;;     辞書定義情報は次のような構成になっています。
;;
;;         (sdic-compat ファイル名 (オプションA 値A) (オプションB 値B) ...)
;;
;;     特別な指定が不要な場合には、オプションは省略できます。
;;
;;         (sdic-compat ファイル名)


;;; Options:

;; sdic-compat.el に対して指定できるオプションは次の通りです。
;;
;; coding-system
;;     辞書の漢字コードを指定します。省略した場合は、
;;     sdic-default-coding-system の値を使います。
;;
;; title
;;     辞書のタイトルを指定します。省略した場合は、辞書ファイルの 
;;     basename をタイトルとします。
;;
;; look
;;     前方一致検索/完全一致検索の時に利用する外部コマンドの名前を指定
;;     します。省略した場合は sdic-compat-look-command の値を使います。
;;
;; look-case-option
;;     look オプションによって指定された外部コマンドに対して、英大文字
;;     /小文字を区別しないで検索するように指示するためのコマンドライン
;;     引数を指定します。省略した場合は sdic-compat-look-case-option の
;;     値を使います。
;;
;; grep
;;     後方一致検索/全文検索の時に利用する外部コマンドの名前を指定しま
;;     す。省略した場合は sdic-compat-grep-command の値を使います。
;;
;; grep-case-option
;;     grep オプションによって指定された外部コマンドに対して、英大文字
;;     /小文字を区別しないで検索するように指示するためのコマンドライン
;;     引数を指定します。省略した場合は sdic-compat-grep-case-option の
;;     値を使います。
;;
;; egrep
;;     正規表現検索の時に利用する外部コマンドの名前を指定します。省略
;;     した場合は sdic-compat-egrep-command の値を使います。
;;
;; egrep-case-option
;;     egrep オプションによって指定された外部コマンドに対して、英大文
;;     字/小文字を区別しないで検索するように指示するためのコマンドライ
;;     ン引数を指定します。省略した場合は 
;;     sdic-compat-egrep-case-option の値を使います。


;;; Note:

;; sdic-compat-look-command / sdic-compat-grep-command /
;; sdic-compat-egrep-command の値は自動的に設定されます。例えば、
;; sdic-compat-grep-command の場合、fgrep / fgrep.exe / grep /
;; grep.exe と4種のコマンドを検索して、見つかったコマンドを使います。
;;
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
(provide 'sdic-compat)
(put 'sdic-compat 'version "2.0")
(put 'sdic-compat 'init-dictionary 'sdic-compat-init-dictionary)
(put 'sdic-compat 'open-dictionary 'sdic-compat-open-dictionary)
(put 'sdic-compat 'close-dictionary 'sdic-compat-close-dictionary)
(put 'sdic-compat 'search-entry 'sdic-compat-search-entry)
(put 'sdic-compat 'get-content 'sdic-compat-get-content)


;;;----------------------------------------------------------------------
;;;		定数/変数の宣言
;;;----------------------------------------------------------------------

(defvar sdic-compat-look-command nil "*Executable file name of look")

(defvar sdic-compat-look-case-option "-f" "*Command line option for look to ignore case")

(defvar sdic-compat-grep-command nil "*Executable file name of grep")

(defvar sdic-compat-grep-case-option "-i" "*Command line option for grep to ignore case")

(defvar sdic-compat-egrep-command nil "*Executable file name of egrep")

(defvar sdic-compat-egrep-case-option "-i" "*Command line option for egrep to ignore case")

(defconst sdic-compat-search-buffer-name " *sdic-compat*")



;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

;; sdic-compat-*-command の初期値を設定
(mapcar '(lambda (list)
	   (or (symbol-value (car list))
	       (set (car list)
		    (catch 'which
		      (mapcar '(lambda (file)
				 (mapcar '(lambda (path)
					    (if (file-executable-p (expand-file-name file path))
						(throw 'which (expand-file-name file path))))
					 exec-path))
			      (cdr list))
		      nil))))
	'((sdic-compat-look-command "look" "look.exe")
	  (sdic-compat-grep-command "fgrep" "fgrep.exe" "grep" "grep.exe")
	  (sdic-compat-egrep-command "egrep" "egrep.exe" "grep" "grep.exe")))


(defun sdic-compat-available-p () "\
Function to check availability of library.
ライブラリの利用可能性を検査する関数"
  (and (stringp sdic-compat-look-command) (stringp sdic-compat-grep-command)))


(defun sdic-compat-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (sdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "sdic-compat+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (or (get dic 'look)
	      (put dic 'look sdic-compat-look-command))
	  (or (get dic 'look-case-option)
	      (put dic 'look-case-option sdic-compat-look-case-option))
	  (or (get dic 'grep)
	      (put dic 'grep sdic-compat-grep-command))
	  (or (get dic 'grep-case-option)
	      (put dic 'grep-case-option sdic-compat-grep-case-option))
	  (or (get dic 'egrep)
	      (put dic 'egrep sdic-compat-egrep-command))
	  (or (get dic 'egrep-case-option)
	      (put dic 'egrep-case-option sdic-compat-egrep-case-option))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system sdic-default-coding-system))
	  (and (stringp (get dic 'look))
	       (stringp (get dic 'grep))
	       dic))
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun sdic-compat-open-dictionary (dic)
  "Function to open dictionary"
  (and (or (sdic-buffer-live-p (get dic 'sdic-compat-search-buffer))
	   (put dic 'sdic-compat-search-buffer (generate-new-buffer sdic-compat-search-buffer-name)))
       dic))


(defun sdic-compat-close-dictionary (dic)
  "Function to close dictionary"
  (kill-buffer (get dic 'sdic-compat-search-buffer))
  (put dic 'sdic-compat-search-buffer nil))


(defun sdic-compat-search-entry (dic string &optional search-type) "\
Function to search word with look or grep, and write results to current buffer.
search-type の値によって次のように動作を変更する。
    nil    : 前方一致検索
    t      : 後方一致検索
    lambda : 完全一致検索
    0      : 全文検索
    regexp : 正規表現検索
検索結果として見つかった見出し語をキーとし、その定義文の先頭の point を値とする
連想配列を返す。"
  (save-excursion
    (set-buffer (get dic 'sdic-compat-search-buffer))
    (save-restriction
      (if (get dic 'sdic-compat-erase-buffer)
	  (delete-region (point-min) (point-max))
	(goto-char (point-max))
	(narrow-to-region (point-max) (point-max)))
      (put dic 'sdic-compat-erase-buffer nil)
      (cond
       ;; 前方一致検索の場合 -> look を使って検索
       ((eq search-type nil)
	(if (string-match "\\Ca" string)
	    (sdic-call-process (get dic 'look) nil t nil
			       (get dic 'coding-system)
			       string (get dic 'file-name))
	  (sdic-call-process (get dic 'look) nil t nil
			     (get dic 'coding-system)
			     (get dic 'look-case-option) string (get dic 'file-name))))
       ;; 後方一致検索の場合 -> grep を使って検索
       ((eq search-type t)
	(if (string-match "\\Ca" string)
	    (sdic-call-process (get dic 'grep) nil t nil
			       (get dic 'coding-system)
			       (concat string "\t") (get dic 'file-name))
	  (sdic-call-process (get dic 'grep) nil t nil
			     (get dic 'coding-system)
			     (get dic 'grep-case-option)
			     (concat string "\t") (get dic 'file-name))))
       ;; 完全一致検索の場合 -> look を使って検索 / 余分なデータを消去
       ((eq search-type 'lambda)
	(if (string-match "\\Ca" string)
	    (sdic-call-process (get dic 'look) nil t nil
			       (get dic 'coding-system)
			       string (get dic 'file-name))
	  (sdic-call-process (get dic 'look) nil t nil
			     (get dic 'coding-system)
			     (get dic 'look-case-option)
			     string (get dic 'file-name)))
	(goto-char (point-min))
	(while (if (looking-at (format "%s\t" (regexp-quote string)))
		   (= 0 (forward-line 1))
		 (delete-region (point) (point-max)))))
       ;; 全文検索の場合 -> grep を使って検索
       ((eq search-type 0)
	(if (string-match "\\Ca" string)
	    (sdic-call-process (get dic 'grep) nil t nil
			       (get dic 'coding-system)
			       string (get dic 'file-name))
	  (sdic-call-process (get dic 'grep) nil t nil
			     (get dic 'coding-system)
			     (get dic 'grep-case-option)
			     string (get dic 'file-name))))
       ;; 正規表現検索の場合 -> egrep を使って検索
       ((eq search-type 'regexp)
	(or (stringp (get dic 'egrep))
	    (error "%s" "Command to search regular expression pattern is not specified"))
	(if (string-match "\\Ca" string)
	    (sdic-call-process (get dic 'egrep) nil t nil
			       (get dic 'coding-system)
			       string (get dic 'file-name))
	  (sdic-call-process (get dic 'egrep) nil t nil
			     (get dic 'coding-system)
			     (get dic 'egrep-case-option)
			     string (get dic 'file-name))))
       ;; それ以外の検索形式を指定された場合
       (t (error "Not supported search type is specified. \(%s\)"
		 (prin1-to-string search-type))))
      ;; 各検索結果に ID を付与する
      (goto-char (point-min))
      (let (ret)
	(while (if (looking-at "\\([^\t]+\\)\t")
		   (progn
		     (setq ret (cons (cons (sdic-match-string 1) (match-end 0)) ret))
		     (= 0 (forward-line 1)))))
	(nreverse ret)))))


(defun sdic-compat-get-content (dic point)
  (save-excursion
    (set-buffer (get dic 'sdic-compat-search-buffer))
    (put dic 'sdic-compat-erase-buffer t)
    (if (<= point (point-max))
	(buffer-substring (goto-char point) (progn (end-of-line) (point)))
      (error "Can't find content. (ID=%d)" point))))
