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

(defun js-ts-defs (root-node)
  "Extract JavaScript definitions from ROOT-NODE using tree-sitter.
ROOT-NODE should be a tree-sitter root node."
  ;; TODO: Implement definition extraction
  )

(provide 'js-ts-defs)

;;; js-ts-defs.el ends here
