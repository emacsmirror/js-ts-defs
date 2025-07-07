;;; js-ts-defs.el --- Find JavaScript variable definitions using tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jackson Ray Hamilton

;; Author: Jackson Ray Hamilton <jackson@jacksonrayhamilton.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, javascript, tree-sitter
;; URL: https://github.com/jacksonrayhamilton/js-ts-defs

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

;; This package provides functionality to find JavaScript function and variable
;; definitions using tree-sitter.

;;; Code:

(require 'treesit)

(defun js-ts-defs (root-node)
  "Extract JavaScript definitions from ROOT-NODE using tree-sitter.
ROOT-NODE should be a tree-sitter root node.
Returns a nested scope structure with variable definitions."
  (let ((scope (js-ts-defs--build-scope "file")))
    (js-ts-defs--process-node root-node scope)
    scope))

(defun js-ts-defs--build-scope (scope-type)
  "Build a scope structure from NODE of SCOPE-TYPE.
SCOPE-TYPE can be `file', `function', etc."
  (list :type scope-type
        :variables (make-hash-table :test 'equal)
        :children '()))

(defun js-ts-defs--process-node (node scope)
  "Process NODE and add definitions to SCOPE, recursively processing children."
  (let ((node-type (treesit-node-type node)))
    (cond
     ;; Function declarations create new scopes
     ((or (string= node-type "function_declaration")
          (string= node-type "function_expression"))
      (js-ts-defs--process-function node scope))

     ;; Variable declarations
     ((string= node-type "variable_declaration")
      (js-ts-defs--process-variable-declaration node scope))

     ;; Lexical declarations (let, const)
     ((string= node-type "lexical_declaration")
      (js-ts-defs--process-lexical-declaration node scope))

     ;; For other nodes, just process children
     (t
      (js-ts-defs--process-children node scope)))))

(defun js-ts-defs--process-function (node scope)
  "Process a function NODE, creating a new child scope."
  (let* ((node-type (treesit-node-type node))
         (function-scope (js-ts-defs--build-scope "function"))
         (parameters (js-ts-defs--get-function-parameters node)))

    ;; Handle function names based on node type
    (let ((name-node (treesit-node-child-by-field-name node "name")))
      (when (and name-node (string= (treesit-node-type name-node) "identifier"))
        (let ((name (substring-no-properties (treesit-node-text name-node)))
              (pos (treesit-node-start name-node)))
          (cond
           ;; For function_declaration, add name to current scope
           ((string= node-type "function_declaration")
            (js-ts-defs--add-variable scope name pos))
           ;; For function_expression, add name to function's own scope
           ((string= node-type "function_expression")
            (js-ts-defs--add-variable function-scope name pos))))))

    ;; Add parameters to the function scope
    (dolist (param parameters)
      (js-ts-defs--add-variable function-scope (car param) (cdr param)))

    ;; Process the function body in the new scope
    (let ((body (js-ts-defs--get-function-body node)))
      (when body
        (js-ts-defs--process-node body function-scope)))

    ;; Add the function scope as a child of the current scope
    (push function-scope (plist-get scope :children))))

(defun js-ts-defs--process-variable-declaration (node scope)
  "Process a variable declaration NODE and add variables to SCOPE."
  (let ((declarators (treesit-node-children node)))
    (dolist (child declarators)
      (when (string= (treesit-node-type child) "variable_declarator")
        (let ((identifier (treesit-node-child child 0)))
          (when (and identifier (string= (treesit-node-type identifier) "identifier"))
            (let ((name (substring-no-properties (treesit-node-text identifier)))
                  (pos (treesit-node-start identifier)))
              (js-ts-defs--add-variable scope name pos))))))))

(defun js-ts-defs--process-lexical-declaration (node scope)
  "Process a lexical declaration NODE (let/const) and add variables to SCOPE."
  (let ((declarators (treesit-node-children node)))
    (dolist (child declarators)
      (when (string= (treesit-node-type child) "variable_declarator")
        (let ((identifier (treesit-node-child child 0)))
          (when (and identifier (string= (treesit-node-type identifier) "identifier"))
            (let ((name (substring-no-properties (treesit-node-text identifier)))
                  (pos (treesit-node-start identifier)))
              (js-ts-defs--add-variable scope name pos))))))))

(defun js-ts-defs--get-function-parameters (node)
  "Extract parameter names and positions from function NODE."
  (let ((params '())
        (formal-params (treesit-node-child-by-field-name node "parameters")))
    (when formal-params
      (let ((param-nodes (treesit-node-children formal-params)))
        (dolist (param-node param-nodes)
          (when (string= (treesit-node-type param-node) "identifier")
            (let ((name (substring-no-properties (treesit-node-text param-node)))
                  (pos (treesit-node-start param-node)))
              (push (cons name pos) params))))))
    (nreverse params)))

(defun js-ts-defs--get-function-body (node)
  "Get the body node of a function NODE."
  (treesit-node-child-by-field-name node "body"))

(defun js-ts-defs--add-variable (scope name position)
  "Add a variable NAME at POSITION to SCOPE if not already defined."
  (let ((variables (plist-get scope :variables)))
    (unless (gethash name variables)
      (puthash name position variables))))

(defun js-ts-defs--process-children (node scope)
  "Process all children of NODE in the current SCOPE."
  (let ((children (treesit-node-children node)))
    (dolist (child children)
      (js-ts-defs--process-node child scope))))

(provide 'js-ts-defs)

;;; js-ts-defs.el ends here
