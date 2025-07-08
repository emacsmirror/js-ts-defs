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

(provide 'js-ts-defs-test)

;;; js-ts-defs-test.el ends here
