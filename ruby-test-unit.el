;;; ruby-test-unit.el --- run Ruby Test::Unit test case in Emacs compilation-mode

;; Copyright (C) 2019 by Yoshinori Toki

;; Author: Yoshinori Toki <toki@freedom.ne.jp>
;; Version: 0.1
;; Package-Requires: ((compile) (ruby-mode))
;; Keywords: ruby, test
;; URL: https://github.com/y10k/ruby-test-unit.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Usage:
;;
;; modity .emacs to set ruby-test-unit key definition in ruby-mode.
;;
;;   (require 'ruby-test-unit)
;;   (add-hook 'ruby-mode-hook
;;             (lambda () (ruby-test-unit-keys)))
;;
;; Key bindings in ruby-mode:
;;   
;;   C-c .  ruby-test-unit-run-test-method
;;   C-c @  ruby-test-unit-run-test-class
;;   C-c f  ruby-test-unit-run-test-file
;;   C-c r  ruby-test-unit-run-rake-test
;;   C-c c  compile (use to run the last test again)
;;

;;; Code:

(require 'compile)
(require 'ruby-mode)

(defvar ruby-test-unit-ruby-command "bundle exec ruby"
  "Ruby command to run test of Ruby Test::Unit at 'compilation-mode'.")

(defvar ruby-test-unit-rake-test-command "bundle exec rake test"
  "Rake command to run test task at 'compilation-mode'.")

(defvar ruby-test-unit-runner-options nil
  "Command options of Ruby Test::Unit.")

(defvar ruby-test-unit-method-definition-regexp
  '((pattern . "\\(?:^\\|\\s-\\)def\\s-+\\(test_[^[:space:](){}?!]+[?!]?\\)")
    (name-pos . 1)))

(defvar ruby-test-unit-class-definition-regexp
  '((pattern . "\\(?:^\\|\\s-\\)class\\s-+\\([[:upper:]]\\S-*\\)\\s-*<\\s-*Test::Unit::TestCase")
    (name-pos . 1)))

(defun ruby-test-unit-get-test-file-name ()
  "Return the name of the test file opened in the current buffer."
  (let ((file-name (buffer-file-name)))
    (if file-name
        (if (string-match ".*\\.[Rr][Bb]$" file-name)
            file-name))))

(defun ruby-test-unit-get-point-at-beginning-of-line ()
  "Move to the head of the line at current point of current buffer and return the point."
  (beginning-of-line)
  (point))

(defun ruby-test-unit-get-point-at-end-of-line ()
  "Move to the end of the line at current point of current buffer and return the point."
  (end-of-line)
  (point))

(defun ruby-test-unit-get-line ()
  "Return a line of current point of the current buffer as a string."
  (buffer-substring-no-properties
   (ruby-test-unit-get-point-at-beginning-of-line)
   (ruby-test-unit-get-point-at-end-of-line)))

(defun ruby-test-unit-goto-test-method-definition ()
  "Move to the test method definition line."
  (end-of-line)                         ; to include the current line as a search target
  (let ((case-fold-search nil))
    (re-search-backward (cdr (assq 'pattern ruby-test-unit-method-definition-regexp)) nil t)))

(defun ruby-test-unit-goto-test-class-definition ()
  "Move to the test class definition line."
  (end-of-line)                         ; to include the current line as a search target
  (let ((case-fold-search nil))
    (re-search-backward (cdr (assq 'pattern ruby-test-unit-class-definition-regexp)) nil t)))

(defun ruby-test-unit-get-test-method-name (line)
  "Retrieve the name of the test method from the test method definition line string."
  (let ((case-fold-search nil))
    (if (string-match (cdr (assq 'pattern ruby-test-unit-method-definition-regexp)) line)
        (match-string (cdr (assq 'name-pos ruby-test-unit-method-definition-regexp)) line))))

(defun ruby-test-unit-get-test-class-name (line)
  "Retrieve the name of the test class from the test class definition line string."
  (let ((case-fold-search nil))
    (if (string-match (cdr (assq 'pattern ruby-test-unit-class-definition-regexp)) line)
        (match-string (cdr (assq 'name-pos ruby-test-unit-class-definition-regexp)) line))))

(defun ruby-test-unit-get-test-file-command-string (test-file-name &optional test-options ruby-options)
  "Return the command string to execute the test file."
  (concat ruby-test-unit-ruby-command
          (if ruby-options (concat  " " ruby-options) "")
          " " (shell-quote-argument test-file-name)
          (if test-options (concat " " test-options) "")))

(defun ruby-test-unit-get-test-class-command-string (test-file-name test-class-name &optional test-options ruby-options)
  "Return the command string to execute the test class."
  (concat (ruby-test-unit-get-test-file-command-string test-file-name
                                                       test-options
                                                       ruby-options)
          " " (shell-quote-argument (concat "-t/\\b" test-class-name "\\z/"))))

(defun ruby-test-unit-get-test-method-command-string (test-file-name test-class-name test-method-name &optional test-options ruby-options)
  "Return the command string to execute the test method."
  (concat (ruby-test-unit-get-test-class-command-string test-file-name
                                                        test-class-name
                                                        test-options
                                                        ruby-options)
          " " (shell-quote-argument (concat "-n" test-method-name))))

;;;#autoload
(defun ruby-test-unit-run-test-method (ruby-debug-option-p)
  "Run test method of Ruby Test::Unit at 'compilation-mode'."
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

;;;#autoload
(defun ruby-test-unit-run-test-class (ruby-debug-option-p)
  "Run test class of Ruby Test::Unit at 'compilation-mode'."
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

;;;#autoload
(defun ruby-test-unit-run-test-file (ruby-debug-option-p)
  "Run test file of Ruby Test::Unit at 'compilation-mode'."
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

;;;#autoload
(defun ruby-test-unit-run-rake-test ()
  "Run test task of Rake at 'compilation-mode'."
  (interactive)
  (compile (concat ruby-test-unit-rake-test-command
                   (if ruby-test-unit-runner-options
                       (concat " " (shell-quote-argument (concat "TESTOPTS=" ruby-test-unit-runner-options)))
                     ""))))

;;;#autoload
(defun ruby-test-unit-keys ()
  "Set local key defs for ruby-test-unit in 'ruby-mode'."
  (define-key ruby-mode-map (kbd "C-c .") 'ruby-test-unit-run-test-method)
  (define-key ruby-mode-map (kbd "C-c @") 'ruby-test-unit-run-test-class)
  (define-key ruby-mode-map (kbd "C-c f") 'ruby-test-unit-run-test-file)
  (define-key ruby-mode-map (kbd "C-c r") 'ruby-test-unit-run-rake-test)
  (define-key ruby-mode-map (kbd "C-c c") 'compile))

(defun ruby-test-unit-compilation-errors ()
  "Set error defs for ruby-test-unit in 'compilation-mode'."
  (dolist (i '((ruby-1 "\\s-*\\[?\\(\\S-+\\):\\([0-9]+\\)\\(?::in\\|$\\)" 1 2)
               (ruby-2 "\\s-*from \\(\\S-+\\):\\([0-9]+\\)\\(?::in\\|$\\)" 1 2)
               (ruby-3 "\\[\\(\\S-+\\):\\([0-9]+\\)\\]:$" 1 2)))
    (add-to-list 'compilation-error-regexp-alist (car i))
    (add-to-list 'compilation-error-regexp-alist-alist i)))

(ruby-test-unit-compilation-errors)

(provide 'ruby-test-unit)

;;; ruby-test-unit.el ends here

;; Local Variables:
;; mode: Emacs-Lisp
;; indent-tabs-mode: nil
;; End:
