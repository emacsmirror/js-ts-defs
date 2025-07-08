;;; js-ts-defs-test.el --- Tests for js-ts-defs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jackson Ray Hamilton

;; Author: Jackson Ray Hamilton <jackson@jacksonrayhamilton.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for js-ts-defs package.

;;; Code:

(require 'ert)
(require 'js-ts-defs)

(defun js-ts-defs--deep-equal (obj1 obj2)
  "Deep equality comparison that handles hash tables.
Compare OBJ1 with OBJ2.
Like `equal' but also compares hash table contents."
  (cond
   ;; Both are hash tables
   ((and (hash-table-p obj1) (hash-table-p obj2))
    (and (= (hash-table-count obj1) (hash-table-count obj2))
         (eq (hash-table-test obj1) (hash-table-test obj2))
         (catch 'not-equal
           (maphash (lambda (key value1)
                      (let ((value2 (gethash key obj2 'js-ts-defs--not-found)))
                        (when (or (eq value2 'js-ts-defs--not-found)
                                  (not (js-ts-defs--deep-equal value1 value2)))
                          (throw 'not-equal nil))))
                    obj1)
           t)))

   ;; One is hash table, other is not
   ((or (hash-table-p obj1) (hash-table-p obj2))
    nil)

   ;; Both are lists
   ((and (listp obj1) (listp obj2))
    (and (= (length obj1) (length obj2))
         (catch 'not-equal
           (while (and obj1 obj2)
             (unless (js-ts-defs--deep-equal (car obj1) (car obj2))
               (throw 'not-equal nil))
             (setq obj1 (cdr obj1)
                   obj2 (cdr obj2)))
           t)))

   ;; One is list, other is not
   ((or (listp obj1) (listp obj2))
    nil)

   ;; Use regular equal for everything else
   (t
    (equal obj1 obj2))))

(ert-deftest js-ts-defs-test-build-scope ()
  "Test building scope object from buffer with var/let/const declarations."
  (with-temp-buffer
    (insert "var globalVar = 1;\n")
    (insert "let globalLet = 2;\n")
    (insert "function myFunc(param1) {\n")
    (insert "  var localVar = 3;\n")
    (insert "  let localLet = 4;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           ;; Create expected scope structure manually
           (expected-variables (make-hash-table :test 'equal))
           (expected-func-variables (make-hash-table :test 'equal))
           (expected-func-scope nil)
           (expected-scope nil))

      ;; Build expected global variables hash table
      (puthash "globalVar" 5 expected-variables)  ; position of "globalVar"
      (puthash "globalLet" 24 expected-variables) ; position of "globalLet"
      (puthash "myFunc" 48 expected-variables)    ; position of "myFunc"

      ;; Build expected function variables hash table
      (puthash "param1" 55 expected-func-variables) ; position of "param1"
      (puthash "localVar" 71 expected-func-variables) ; position of "localVar"
      (puthash "localLet" 91 expected-func-variables) ; position of "localLet"

      ;; Build expected function scope
      (setq expected-func-scope
            (list :type "function"
                  :start 39  ; start of function
                  :end 106   ; end of function
                  :variables expected-func-variables
                  :children '()))

      ;; Build expected global scope
      (setq expected-scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 107     ; end of buffer
                  :variables expected-variables
                  :children (list expected-func-scope)))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-jump-to-definition ()
  "Test jumping to variable and function definitions."
  (with-temp-buffer
    (insert "function greet(name) {\n")
    (insert "  let message = 'Hello, ' + name;\n")
    (insert "  return message;\n")
    (insert "}\n")
    (insert "\n")
    (insert "let result = greet('World');\n")

    ;; Enable js-ts-mode
    (js-ts-mode)

    ;; Test 1: Jump from 'message' usage to its definition
    (goto-char (point-min))
    (search-forward "return message")
    (backward-word)  ; Move to start of 'message'
    (let ((usage-pos (point)))
      (js-ts-defs-jump-to-definition)
      (should (< (point) usage-pos))  ; Should jump backward to definition
      (should (looking-at "message")))

    ;; Test 2: Jump from function call to function definition
    (goto-char (point-min))
    (search-forward "greet('World')")
    (backward-word 2)  ; Move to start of 'greet'
    (let ((call-pos (point)))
      (js-ts-defs-jump-to-definition)
      (should (< (point) call-pos))  ; Should jump backward to definition
      (should (looking-at "greet")))

    ;; Test 3: Jump from parameter usage to parameter definition
    (goto-char (point-min))
    (search-forward "'Hello, ' + name")
    (backward-word)  ; Move to start of 'name'
    (let ((usage-pos (point)))
      (js-ts-defs-jump-to-definition)
      (should (< (point) usage-pos))  ; Should jump backward to parameter
      (should (looking-at "name")))))

(ert-deftest js-ts-defs-test-no-tree-sitter-parser ()
  "Test error when no tree-sitter parser is available."
  (with-temp-buffer
    (insert "let x = 42;")
    ;; Don't enable js-ts-mode, so no parser will be available
    (let ((err (should-error (js-ts-defs-jump-to-definition)
                             :type 'user-error)))
      (should (string= (cadr err) "No tree-sitter parser available")))))

(ert-deftest js-ts-defs-test-no-identifier-at-point ()
  "Test error when point is not on an identifier."
  (with-temp-buffer
    (insert "let x = 42;")
    (js-ts-mode)
    ;; Position point on the '=' character
    (goto-char (point-min))
    (search-forward "=")
    (backward-char)
    (let ((err (should-error (js-ts-defs-jump-to-definition)
                             :type 'user-error)))
      (should (string= (cadr err) "No identifier at point")))))

(ert-deftest js-ts-defs-test-definition-not-found ()
  "Test error when definition cannot be found for an identifier."
  (with-temp-buffer
    (insert "console.log(notDefined);")
    (js-ts-mode)
    ;; Position point on 'notDefined'
    (goto-char (point-min))
    (search-forward "notDefined")
    (backward-word)
    (let ((err (should-error (js-ts-defs-jump-to-definition)
                             :type 'user-error)))
      (should (string= (cadr err)
                       (format-message "Definition not found for `notDefined'"))))))

(ert-deftest js-ts-defs-test-arguments-dynamic-scope ()
  "Test error for 'arguments' identifier in non-arrow function."
  (with-temp-buffer
    (insert "function test() {\n")
    (insert "  console.log(arguments);\n")
    (insert "}")
    (js-ts-mode)
    ;; Position point on 'arguments'
    (goto-char (point-min))
    (search-forward "arguments")
    (backward-word)
    (let ((err (should-error (js-ts-defs-jump-to-definition)
                             :type 'user-error)))
      (should (string= (cadr err)
                       (format-message "`arguments' has dynamic scope"))))))

(ert-deftest js-ts-defs-test-arguments-in-arrow-function ()
  "Test that 'arguments' in arrow function gives definition not found error."
  (with-temp-buffer
    (insert "const test = () => {\n")
    (insert "  console.log(arguments);\n")
    (insert "};")
    (js-ts-mode)
    ;; Position point on 'arguments'
    (goto-char (point-min))
    (search-forward "arguments")
    (backward-word)
    (let ((err (should-error (js-ts-defs-jump-to-definition)
                             :type 'user-error)))
      (should (string= (cadr err)
                       (format-message "Definition not found for `arguments'"))))))

(provide 'js-ts-defs-test)

;;; js-ts-defs-test.el ends here
