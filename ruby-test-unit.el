;;; ruby-unit-test.el --- Emacs からコンパイルモードでTest::Unitのテストケースを実行する。

(require 'compile)
(require 'ruby-mode)

(defvar ruby-test-unit-ruby-command "bundle exec ruby"
  "Rubyを実行するコマンドを設定する。")

(defvar ruby-test-unit-rake-test-command "bundle exec rake test"
  "Rakeでtestタスクを実行するコマンドを設定する。")

(defvar ruby-test-unit-runner-options nil
  "TestRunnerのオプションを設定する。")

(defvar ruby-test-unit-method-definition-regexp
  '((pattern . "\\(?:^\\|\\s-\\)def\\s-+\\(test_[^ \t(){}?!]+[?!]?\\)")
    (name-pos . 1)))

(defvar ruby-test-unit-class-definition-regexp
  '((pattern . "\\(?:^\\|\\s-\\)class\\s-+\\(\\S-+\\)\\s-*<\\s-*Test::Unit::TestCase")
    (name-pos . 1)))

(defun ruby-test-unit-get-test-file-name ()
  "カレントバッファで開いているテストファイルの名前を返す。"
  (let ((file-name (buffer-file-name)))
    (if file-name
        (if (string-match ".*\\.[Rr][Bb]$" file-name)
            file-name))))

(defun ruby-test-unit-get-point-at-beginning-of-line ()
  "ポイントをカレントバッファの行頭に移動してポイント値を返す。"
  (beginning-of-line)
  (point))

(defun ruby-test-unit-get-point-at-end-of-line ()
  "ポイントをカレントバッファの行末に移動してポイント値を返す。"
  (end-of-line)
  (point))

(defun ruby-test-unit-get-line ()
  "カレントバッファのポイントの1行を文字列で返す。"
  (buffer-substring-no-properties
   (ruby-test-unit-get-point-at-beginning-of-line)
   (ruby-test-unit-get-point-at-end-of-line)))

(defun ruby-test-unit-goto-test-method-definition ()
  "メソッド定義の行へ移動する。"
  (end-of-line)                         ;カレント行を検索対象に含めるため
  (let ((case-fold-search nil))
    (re-search-backward (cdr (assq 'pattern ruby-test-unit-method-definition-regexp)) nil t)))

(defun ruby-test-unit-goto-test-class-definition ()
  "クラス定義の行へ移動する。"
  (end-of-line)                         ;カレント行を検索対象に含めるため
  (let ((case-fold-search nil))
    (re-search-backward (cdr (assq 'pattern ruby-test-unit-class-definition-regexp)) nil t)))

(defun ruby-test-unit-get-test-method-name (line)
  "Ruby Test::Unitのテストメソッドの名前を文字列から取り出して返す。"
  (let ((case-fold-search nil))
    (if (string-match (cdr (assq 'pattern ruby-test-unit-method-definition-regexp)) line)
        (match-string (cdr (assq 'name-pos ruby-test-unit-method-definition-regexp)) line))))

(defun ruby-test-unit-get-test-class-name (line)
  "Ruby Test::Unitのテストクラスの名前を文字列から取り出して返す。"
  (let ((case-fold-search nil))
    (if (string-match (cdr (assq 'pattern ruby-test-unit-class-definition-regexp)) line)
        (match-string (cdr (assq 'name-pos ruby-test-unit-class-definition-regexp)) line))))

(defun ruby-test-unit-get-test-file-command-string (test-file-name &optional test-options ruby-options)
  "Ruby Test::Unitのテストファイルを実行するコマンドを文字列で返す。"
  (concat ruby-test-unit-ruby-command " "
          (if ruby-options (concat ruby-options " ") "")
          (shell-quote-argument test-file-name)
          (if test-options (concat " " test-options) "")))

(defun ruby-test-unit-get-test-class-command-string (test-file-name test-class-name &optional test-options ruby-options)
  "Ruby Test::Unitのテストクラスを実行するコマンドを文字列で返す。"
  (concat (ruby-test-unit-get-test-file-command-string test-file-name
                                                       test-options
                                                       ruby-options)
          " "
          (shell-quote-argument (concat "-t/\\b" test-class-name "\\b/"))))

(defun ruby-test-unit-get-test-method-command-string (test-file-name test-class-name test-method-name &optional test-options ruby-options)
  "Ruby Test::Unitのテストメソッドを実行するコマンドを文字列で返す。"
  (concat (ruby-test-unit-get-test-class-command-string test-file-name
                                                        test-class-name
                                                        test-options
                                                        ruby-options)
          " "
          (shell-quote-argument (concat "-n" test-method-name))))

(defun ruby-test-unit-run-test-method (ruby-debug-option-p)
  "run test method of Ruby Test::Unit at compilation mode."
  (interactive "P")
  (save-excursion
    (let ((test-file-name (ruby-test-unit-get-test-file-name)))
      (if test-file-name
          (if (ruby-test-unit-goto-test-method-definition)
              (let ((test-method-name (ruby-test-unit-get-test-method-name (ruby-test-unit-get-line))))
                (if (ruby-test-unit-goto-test-class-definition)
                    (let ((test-class-name (ruby-test-unit-get-test-class-name (ruby-test-unit-get-line))))
                      (let ((command-string
                             (if ruby-debug-option-p
                                 (ruby-test-unit-get-test-method-command-string test-file-name
                                                                                test-class-name
                                                                                test-method-name
                                                                                ruby-test-unit-runner-options
                                                                                "-d")
                               (ruby-test-unit-get-test-method-command-string test-file-name
                                                                              test-class-name
                                                                              test-method-name
                                                                              ruby-test-unit-runner-options))))
                        (compile command-string)))
                  (message "Not found a Ruby Test::Unit test-case class.")))
            (message "Not found a Ruby Test::Unit method."))
        (message "Not a ruby script file.")))))

(defun ruby-test-unit-run-test-class (ruby-debug-option-p)
  "run test class of Ruby Test::Unit at compilation mode."
  (interactive "P")
  (save-excursion
    (let ((test-file-name (ruby-test-unit-get-test-file-name)))
      (if test-file-name
          (if (ruby-test-unit-goto-test-class-definition)
              (let ((test-class-name (ruby-test-unit-get-test-class-name (ruby-test-unit-get-line))))
                (let ((command-string
                       (if ruby-debug-option-p
                           (ruby-test-unit-get-test-class-command-string test-file-name
                                                                         test-class-name
                                                                         ruby-test-unit-runner-options
                                                                         "-d")
                         (ruby-test-unit-get-test-class-command-string test-file-name
                                                                       test-class-name
                                                                       ruby-test-unit-runner-options))))
                  (compile command-string)))
            (message "Not found a Ruby Test::Unit test-case class."))
        (message "Not a ruby script file.")))))

(defun ruby-test-unit-run-test-file (ruby-debug-option-p)
  "run test file of Ruby Test::Unit at compilation mode."
  (interactive "P")
  (save-excursion
    (let ((test-file-name (ruby-test-unit-get-test-file-name)))
      (if test-file-name
          (let ((command-string
                 (if ruby-debug-option-p
                     (ruby-test-unit-get-test-file-command-string test-file-name
                                                                  ruby-test-unit-runner-options
                                                                  "-d")
                   (ruby-test-unit-get-test-file-command-string test-file-name
                                                                ruby-test-unit-runner-options))))
            (compile command-string))
        (message "Not a ruby script file.")))))

(defun ruby-test-unit-run-rake-test ()
  "run test task of Rake at compilation mode."
  (interactive)
  (compile (concat ruby-test-unit-rake-test-command
                   (if ruby-test-unit-runner-options
                       (concat " " (shell-quote-argument (concat "TESTOPTS=" ruby-test-unit-runner-options)))
                     ""))))

(defun ruby-test-unit-keys ()
  "set local key defs for ruby-test-unit in ruby-mode"
  (define-key ruby-mode-map "\C-c." 'ruby-test-unit-run-test-method)
  (define-key ruby-mode-map "\C-c@" 'ruby-test-unit-run-test-class)
  (define-key ruby-mode-map "\C-cf" 'ruby-test-unit-run-test-file)
  (define-key ruby-mode-map "\C-cr" 'ruby-test-unit-run-rake-test)
  (define-key ruby-mode-map "\C-cc" 'compile))

(defun ruby-test-unit-compilation-errors ()
  "set error defs for ruby-test-unit in compilation-mode"
  (dolist (i '((ruby-1 "\\s-*\\[?\\(\\S-+\\):\\([0-9]+\\)\\(?::in\\|$\\)" 1 2)
               (ruby-2 "\\s-*from \\(\\S-+\\):\\([0-9]+\\)\\(?::in\\|$\\)" 1 2)
               (ruby-3 "\\[\\(\\S-+\\):\\([0-9]+\\)\\]:$" 1 2)))
    (add-to-list 'compilation-error-regexp-alist (car i))
    (add-to-list 'compilation-error-regexp-alist-alist i)))

(ruby-test-unit-keys)
(ruby-test-unit-compilation-errors)

(provide 'ruby-test-unit)

; Local Variables:
; mode: Emacs-Lisp
; indent-tabs-mode: nil
; End:
