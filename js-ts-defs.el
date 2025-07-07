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
  (let ((scope (js-ts-defs--build-scope "program"
                                        (treesit-node-start root-node)
                                        (treesit-node-end root-node))))
    (js-ts-defs--process-node root-node scope scope)
    scope))

(defun js-ts-defs--build-scope (scope-type start-pos end-pos)
  "Build a scope structure of SCOPE-TYPE with START-POS and END-POS.
SCOPE-TYPE can be `program', `function', etc."
  (list :type scope-type
        :start start-pos
        :end end-pos
        :variables (make-hash-table :test 'equal)
        :children '()))

(defun js-ts-defs--process-node (node function-scope block-scope)
  "Process NODE and add definitions to appropriate scopes.
FUNCTION-SCOPE is the enclosing function/program scope for variable declarations.
BLOCK-SCOPE is the current block scope for lexical declarations."
  (let ((node-type (treesit-node-type node)))
    (cond
     ;; Function declarations create new scopes
     ((or (string= node-type "function_declaration")
          (string= node-type "function_expression")
          (string= node-type "arrow_function"))
      (js-ts-defs--process-function node function-scope block-scope))

     ;; Variable declarations go to function scope
     ((string= node-type "variable_declaration")
      (js-ts-defs--process-variable-declaration node function-scope block-scope))

     ;; Lexical declarations go to block scope
     ((string= node-type "lexical_declaration")
      (js-ts-defs--process-lexical-declaration node function-scope block-scope))

     ;; Statement blocks that may need block scopes
     ((string= node-type "statement_block")
      (js-ts-defs--process-statement-block node function-scope block-scope))

     ;; Catch clauses that define error variables
     ((string= node-type "catch_clause")
      (js-ts-defs--process-catch-clause node function-scope block-scope))

     ;; For other nodes, just process children
     (t
      (js-ts-defs--process-children node function-scope block-scope)))))

(defun js-ts-defs--process-function (node _parent-function-scope parent-block-scope)
  "Process a function NODE, creating a new child scope."
  (let* ((node-type (treesit-node-type node))
         (function-scope (js-ts-defs--build-scope "function"
                                                  (treesit-node-start node)
                                                  (treesit-node-end node)))
         (parameters (js-ts-defs--get-function-parameters node)))

    ;; Handle function names based on node type
    (let ((name-node (treesit-node-child-by-field-name node "name")))
      (when (and name-node (string= (treesit-node-type name-node) "identifier"))
        (let ((name (substring-no-properties (treesit-node-text name-node)))
              (pos (treesit-node-start name-node)))
          (cond
           ;; For function_declaration, add name to parent block scope
           ((string= node-type "function_declaration")
            (js-ts-defs--add-variable parent-block-scope name pos))
           ;; For function_expression, add name to function's own scope
           ((string= node-type "function_expression")
            (js-ts-defs--add-variable function-scope name pos))))))

    ;; Add parameters to the function scope
    (dolist (param parameters)
      (js-ts-defs--add-variable function-scope (car param) (cdr param)))

    ;; Process the function body in the new scope (function scope serves as both function and block scope)
    (let ((body (js-ts-defs--get-function-body node)))
      (when body
        (if (string= (treesit-node-type body) "statement_block")
            ;; If body is a statement block, process its children directly
            (let ((children (treesit-node-children body)))
              (dolist (child children)
                (js-ts-defs--process-node child function-scope function-scope)))
          ;; Otherwise process the body node normally
          (js-ts-defs--process-node body function-scope function-scope))))

    ;; Add the function scope as a child of the parent block scope
    (setf (plist-get parent-block-scope :children)
          (append (plist-get parent-block-scope :children) (list function-scope)))))

(defun js-ts-defs--process-variable-declaration (node function-scope block-scope)
  "Process a variable declaration NODE and add variables to FUNCTION-SCOPE."
  (let ((declarators (treesit-node-children node)))
    (dolist (child declarators)
      (when (string= (treesit-node-type child) "variable_declarator")
        (let ((identifier (treesit-node-child child 0)))
          (when (and identifier (string= (treesit-node-type identifier) "identifier"))
            (let ((name (substring-no-properties (treesit-node-text identifier)))
                  (pos (treesit-node-start identifier)))
              (js-ts-defs--add-variable function-scope name pos))))
        ;; Process the value part of the declaration if it exists
        (let ((value (treesit-node-child-by-field-name child "value")))
          (when value
            (js-ts-defs--process-node value function-scope block-scope)))))))

(defun js-ts-defs--process-lexical-declaration (node function-scope block-scope)
  "Process a lexical declaration NODE (let/const) and add variables to BLOCK-SCOPE."
  (let ((declarators (treesit-node-children node)))
    (dolist (child declarators)
      (when (string= (treesit-node-type child) "variable_declarator")
        (let ((identifier (treesit-node-child child 0)))
          (when (and identifier (string= (treesit-node-type identifier) "identifier"))
            (let ((name (substring-no-properties (treesit-node-text identifier)))
                  (pos (treesit-node-start identifier)))
              (js-ts-defs--add-variable block-scope name pos))))
        ;; Process the value part of the declaration if it exists
        (let ((value (treesit-node-child-by-field-name child "value")))
          (when value
            (js-ts-defs--process-node value function-scope block-scope)))))))

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

(defun js-ts-defs--process-statement-block (node function-scope block-scope)
  "Process a statement block NODE, creating a block scope if it contains lexical declarations."
  (let ((children (treesit-node-children node))
        (has-lexical-declaration nil))

    ;; Check if this block contains any lexical declarations
    (dolist (child children)
      (when (or (string= (treesit-node-type child) "lexical_declaration")
                (string= (treesit-node-type child) "function_declaration"))
        (setq has-lexical-declaration t)))

    (if has-lexical-declaration
        ;; Create a block scope and process children in it
        (let ((new-block-scope (js-ts-defs--build-scope "block"
                                                        (treesit-node-start node)
                                                        (treesit-node-end node))))
          (dolist (child children)
            (js-ts-defs--process-node child function-scope new-block-scope))
          (setf (plist-get block-scope :children)
                (append (plist-get block-scope :children) (list new-block-scope))))
      ;; No lexical declarations, just process children in current scopes
      (dolist (child children)
        (js-ts-defs--process-node child function-scope block-scope)))))

(defun js-ts-defs--process-catch-clause (node _function-scope block-scope)
  "Process a catch clause NODE, unconditionally creating a new block scope."
  (let ((parameter (treesit-node-child-by-field-name node "parameter"))
        (body (treesit-node-child-by-field-name node "body"))
        (catch-scope (js-ts-defs--build-scope "block"
                                              (treesit-node-start node)
                                              (treesit-node-end node))))

    ;; Add the error parameter to the catch scope if it exists
    (when (and parameter (string= (treesit-node-type parameter) "identifier"))
      (let ((name (substring-no-properties (treesit-node-text parameter)))
            (pos (treesit-node-start parameter)))
        (js-ts-defs--add-variable catch-scope name pos)))

    ;; Process the catch body in the catch scope
    (when body
      (if (string= (treesit-node-type body) "statement_block")
          ;; If body is a statement block, process its children directly
          (let ((children (treesit-node-children body)))
            (dolist (child children)
              (js-ts-defs--process-node child catch-scope catch-scope)))
        ;; Otherwise process the body node normally
        (js-ts-defs--process-node body catch-scope catch-scope)))

    (setf (plist-get block-scope :children)
          (append (plist-get block-scope :children) (list catch-scope)))))

(defun js-ts-defs--process-children (node function-scope block-scope)
  "Process all children of NODE in the current scopes."
  (let ((children (treesit-node-children node)))
    (dolist (child children)
      (js-ts-defs--process-node child function-scope block-scope))))

(provide 'js-ts-defs)

;;; js-ts-defs.el ends here
