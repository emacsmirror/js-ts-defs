;; -*- mode: eask; lexical-binding: t -*-

(package "js-ts-defs"
         "0.1.0"
         "Find JavaScript variable definitions using tree-sitter")

(website-url "https://github.com/jacksonrayhamilton/js-ts-defs")
(keywords "languages" "javascript" "tree-sitter")

(package-file "js-ts-defs.el")

(script "install-grammars" "emacs -batch --eval \"(progn (require 'treesit) (add-to-list 'treesit-language-source-alist (list 'javascript \\\"https://github.com/tree-sitter/tree-sitter-javascript\\\" (if (version< emacs-version \\\"30\\\") \\\"v0.20.1\\\" \\\"v0.23.1\\\"))) (add-to-list 'treesit-language-source-alist (list 'jsdoc \\\"https://github.com/tree-sitter/tree-sitter-jsdoc\\\" (if (version< emacs-version \\\"30\\\") \\\"v0.19.0\\\" \\\"v0.23.2\\\"))) (treesit-install-language-grammar 'javascript) (treesit-install-language-grammar 'jsdoc))\"")
(script "test" "emacs -batch -l js-ts-defs.el -l js-ts-defs-test.el -f ert-run-tests-batch-and-exit")

(source "gnu")

(depends-on "emacs" "29.1")
