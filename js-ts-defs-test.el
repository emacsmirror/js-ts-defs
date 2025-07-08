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
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 107     ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "globalVar" 5 variables)  ; position of "globalVar"
                               (puthash "globalLet" 24 variables) ; position of "globalLet"
                               (puthash "myFunc" 48 variables)    ; position of "myFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 39 ; start of function
                                   :end 106  ; end of function
                                   ;; Build expected function variables hash table
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "param1" 55 variables)   ; position of "param1"
                                                (puthash "localVar" 71 variables) ; position of "localVar"
                                                (puthash "localLet" 91 variables) ; position of "localLet"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-named-function-expression-scope ()
  "Test that named functions are available in function expression scope."
  (with-temp-buffer
    (insert "var fn = function namedFunc(param1, param2) {\n")
    (insert "  return param1 + param2;\n")
    (insert "};\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 76      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "fn" 5 variables)  ; position of "fn"
                               variables)
                  :children (list
                             ;; Build expected function expression scope
                             (list :type "function"
                                   :start 10 ; start of function expression
                                   :end 74   ; end of function expression
                                   ;; Build expected function variables hash table
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "namedFunc" 19 variables) ; position of "namedFunc"
                                                (puthash "param1" 29 variables)    ; position of "param1"
                                                (puthash "param2" 37 variables)    ; position of "param2"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-var-vs-let-const-scoping ()
  "Test that var is added to function scope while let/const is added to block scope."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  {\n")
    (insert "    var varInBlock = 1;\n")
    (insert "    let letInBlock = 2;\n")
    (insert "    const constInBlock = 3;\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 109     ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 108  ; end of function
                                   ;; var should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "varInBlock" 35 variables) ; position of "varInBlock"
                                                variables)
                                   :children (list
                                              ;; Build expected block scope
                                              (list :type "block"
                                                    :start 25 ; start of block
                                                    :end 106  ; end of block
                                                    ;; let/const should be in block scope
                                                    :variables (let ((variables (make-hash-table :test 'equal)))
                                                                 (puthash "letInBlock" 59 variables)   ; position of "letInBlock"
                                                                 (puthash "constInBlock" 85 variables) ; position of "constInBlock"
                                                                 variables)
                                                    :children '())))))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-catch-block-scope ()
  "Test that catch variable and var declarations are scoped to catch block."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  try {\n")
    (insert "    throw new Error('test');\n")
    (insert "  } catch (error) {\n")
    (insert "    var varInCatch = 1;\n")
    (insert "    let letInCatch = 2;\n")
    (insert "    const constInCatch = 3;\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 162     ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 161  ; end of function
                                   ;; Function scope should be empty (var is scoped to catch block)
                                   :variables (make-hash-table :test 'equal)
                                   :children (list
                                              ;; Build expected catch block scope
                                              (list :type "block"
                                                    :start 64 ; start of catch clause
                                                    :end 159  ; end of catch clause
                                                    ;; All variables should be in catch block scope
                                                    :variables (let ((variables (make-hash-table :test 'equal)))
                                                                 (puthash "error" 71 variables)         ; position of "error"
                                                                 (puthash "varInCatch" 88 variables)    ; position of "varInCatch"
                                                                 (puthash "letInCatch" 112 variables)   ; position of "letInCatch"
                                                                 (puthash "constInCatch" 138 variables) ; position of "constInCatch"
                                                                 variables)
                                                    :children '())))))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-class-and-method-scope ()
  "Test that classes are added to block scope and methods create function scopes."
  (with-temp-buffer
    (insert "class MyClass {\n")
    (insert "  constructor(param1) {\n")
    (insert "    this.value = param1;\n")
    (insert "  }\n")
    (insert "  \n")
    (insert "  myMethod(param2) {\n")
    (insert "    let localVar = param2;\n")
    (insert "    return localVar;\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 148     ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "MyClass" 7 variables) ; position of "MyClass"
                               variables)
                  :children (list
                             ;; Build expected constructor method scope
                             (list :type "function"
                                   :start 19 ; start of constructor
                                   :end 69   ; end of constructor
                                   ;; Constructor parameters
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "param1" 31 variables) ; position of "param1"
                                                variables)
                                   :children '())
                             ;; Build expected myMethod scope
                             (list :type "function"
                                   :start 75 ; start of myMethod
                                   :end 145  ; end of myMethod
                                   ;; Method parameters and local variables
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "param2" 84 variables)    ; position of "param2"
                                                (puthash "localVar" 102 variables) ; position of "localVar"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-for-loop-var-scope ()
  "Test that var declarations in for loop are hoisted to function scope."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  for (var i = 0; i < 10; i++) {\n")
    (insert "    var innerVar = i;\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 84      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 83   ; end of function
                                   ;; var declarations should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "i" 34 variables)        ; position of "i"
                                                (puthash "innerVar" 64 variables) ; position of "innerVar"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-for-loop-let-scope ()
  "Test that let declarations in for loop create a new for scope."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  for (let i = 0; i < 10; i++) {\n")
    (insert "    let innerVar = i;\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 84      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 83   ; end of function
                                   ;; Function scope should be empty
                                   :variables (make-hash-table :test 'equal)
                                   :children (list
                                              ;; Build expected for scope
                                              (list :type "for"
                                                    :start 25 ; start of for statement
                                                    :end 81   ; end of for statement
                                                    ;; let declarations should be in for scope
                                                    :variables (let ((variables (make-hash-table :test 'equal)))
                                                                 (puthash "i" 34 variables)        ; position of "i"
                                                                 variables)
                                                    :children (list
                                                               ;; Build expected block scope
                                                               (list :type "block"
                                                                     :start 54 ; start of block
                                                                     :end 81   ; end of block
                                                                     ;; let declarations should be in for scope
                                                                     :variables (let ((variables (make-hash-table :test 'equal)))
                                                                                  (puthash "innerVar" 64 variables) ; position of "innerVar"
                                                                                  variables)
                                                                     :children '())))))))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-for-in-loop-var-scope ()
  "Test that var declarations in for-in loop are hoisted to function scope."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  for (var key in obj) {\n")
    (insert "    var value = obj[key];\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 80      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 79   ; end of function
                                   ;; var declarations should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "key" 34 variables)   ; position of "key"
                                                (puthash "value" 56 variables) ; position of "value"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-for-in-loop-let-scope ()
  "Test that let declarations in for-in loop create a new for scope."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  for (let key in obj) {\n")
    (insert "    let value = obj[key];\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 80      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 79   ; end of function
                                   ;; Function scope should be empty
                                   :variables (make-hash-table :test 'equal)
                                   :children (list
                                              ;; Build expected for scope
                                              (list :type "for"
                                                    :start 25 ; start of for-in statement
                                                    :end 77   ; end of for-in statement
                                                    ;; let declarations should be in for scope
                                                    :variables (let ((variables (make-hash-table :test 'equal)))
                                                                 (puthash "key" 34 variables)   ; position of "key"
                                                                 variables)
                                                    :children (list
                                                               ;; Build expected block scope
                                                               (list :type "block"
                                                                     :start 46 ; start of block
                                                                     :end 77   ; end of block
                                                                     ;; let declarations should be in for scope
                                                                     :variables (let ((variables (make-hash-table :test 'equal)))
                                                                                  (puthash "value" 56 variables) ; position of "value"
                                                                                  variables)
                                                                     :children '())))))))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-array-destructuring-var ()
  "Test that array destructuring in var declarations works correctly."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  var [a, b, ...rest] = [1, 2, 3, 4, 5];\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 66      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 65   ; end of function
                                   ;; var declarations should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "a" 30 variables)    ; position of "a"
                                                (puthash "b" 33 variables)    ; position of "b"
                                                (puthash "rest" 39 variables) ; position of "rest"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-object-destructuring-var ()
  "Test that object destructuring in var declarations works correctly."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  var {x, y: z, ...spread} = obj;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 59      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 58   ; end of function
                                   ;; var declarations should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "x" 30 variables)      ; position of "x"
                                                (puthash "z" 36 variables)      ; position of "z"
                                                (puthash "spread" 42 variables) ; position of "spread"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-array-destructuring-let ()
  "Test that array destructuring in let declarations works correctly."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  {\n")
    (insert "    let [first, second] = arr;\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 64      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 63   ; end of function
                                   ;; Function scope should be empty
                                   :variables (make-hash-table :test 'equal)
                                   :children (list
                                              ;; Build expected block scope
                                              (list :type "block"
                                                    :start 25 ; start of block
                                                    :end 61   ; end of block
                                                    ;; let declarations should be in block scope
                                                    :variables (let ((variables (make-hash-table :test 'equal)))
                                                                 (puthash "first" 36 variables)  ; position of "first"
                                                                 (puthash "second" 43 variables) ; position of "second"
                                                                 variables)
                                                    :children '())))))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-function-param-destructuring ()
  "Test that destructuring in function parameters works correctly."
  (with-temp-buffer
    (insert "function testFunc({a, b: renamed}, [x, y]) {\n")
    (insert "  return a + renamed + x + y;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 78      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 77   ; end of function
                                   ;; Function parameters should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "a" 20 variables)       ; position of "a"
                                                (puthash "renamed" 26 variables) ; position of "renamed"
                                                (puthash "x" 37 variables)       ; position of "x"
                                                (puthash "y" 40 variables)       ; position of "y"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-catch-param-destructuring ()
  "Test that destructuring in catch parameters works correctly."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  try {\n")
    (insert "    throw {code: 500, message: 'Error'};\n")
    (insert "  } catch ({code, message: msg}) {\n")
    (insert "    console.log(code, msg);\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 141     ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 140  ; end of function
                                   ;; Function scope should be empty
                                   :variables (make-hash-table :test 'equal)
                                   :children (list
                                              ;; Build expected catch block scope
                                              (list :type "block"
                                                    :start 76 ; start of catch clause
                                                    :end 138  ; end of catch clause
                                                    ;; Catch parameters should be in catch scope
                                                    :variables (let ((variables (make-hash-table :test 'equal)))
                                                                 (puthash "code" 84 variables) ; position of "code"
                                                                 (puthash "msg" 99 variables)  ; position of "msg"
                                                                 variables)
                                                    :children '())))))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-default-parameters ()
  "Test that default parameters work correctly."
  (with-temp-buffer
    (insert "function testFunc(a = 1, b = 2, c) {\n")
    (insert "  return a + b + c;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 60      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 59   ; end of function
                                   ;; Function parameters should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "a" 19 variables) ; position of "a"
                                                (puthash "b" 26 variables) ; position of "b"
                                                (puthash "c" 33 variables) ; position of "c"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-rest-parameters ()
  "Test that rest parameters work correctly."
  (with-temp-buffer
    (insert "function testFunc(first, ...rest) {\n")
    (insert "  return first + rest.length;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 69      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 68   ; end of function
                                   ;; Function parameters should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "first" 19 variables) ; position of "first"
                                                (puthash "rest" 29 variables)  ; position of "rest"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-nested-destructuring ()
  "Test that nested destructuring patterns work correctly."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  var {a: {b, c}, d: [e, f]} = obj;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 61      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 60   ; end of function
                                   ;; var declarations should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "b" 34 variables) ; position of "b"
                                                (puthash "c" 37 variables) ; position of "c"
                                                (puthash "e" 45 variables) ; position of "e"
                                                (puthash "f" 48 variables) ; position of "f"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-destructuring-with-defaults ()
  "Test that destructuring with default values works correctly."
  (with-temp-buffer
    (insert "function testFunc({a = 1, b: renamed = 2} = {}) {\n")
    (insert "  return a + renamed;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 75      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 74   ; end of function
                                   ;; Function parameters should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "a" 20 variables)       ; position of "a"
                                                (puthash "renamed" 30 variables) ; position of "renamed"
                                                variables)
                                   :children '())))))

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
    (backward-word)                     ; Move to start of 'message'
    (let ((usage-pos (point)))
      (js-ts-defs-jump-to-definition)
      (should (< (point) usage-pos))    ; Should jump backward to definition
      (should (looking-at "message")))

    ;; Test 2: Jump from function call to function definition
    (goto-char (point-min))
    (search-forward "greet('World')")
    (backward-word 2)                   ; Move to start of 'greet'
    (let ((call-pos (point)))
      (js-ts-defs-jump-to-definition)
      (should (< (point) call-pos))     ; Should jump backward to definition
      (should (looking-at "greet")))

    ;; Test 3: Jump from parameter usage to parameter definition
    (goto-char (point-min))
    (search-forward "'Hello, ' + name")
    (backward-word)                     ; Move to start of 'name'
    (let ((usage-pos (point)))
      (js-ts-defs-jump-to-definition)
      (should (< (point) usage-pos))    ; Should jump backward to parameter
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
