# js-ts-defs

Find JavaScript variable definitions using tree-sitter.

## Overview

This package provides `js-ts-defs-jump-to-definition` which jumps to the definition of the JavaScript identifier at point. It uses tree-sitter to parse JavaScript code and build a scope structure to accurately resolve variable definitions.

## Features

- Jump to definitions of variables, functions, classes, and imports
- Handles JavaScript scoping rules (var, let, const)
- Supports destructuring patterns and function parameters
- Works with arrow functions and regular functions
- Handles block scoping and lexical declarations

## Usage

Call `M-x js-ts-defs-jump-to-definition` to jump to the definition of the identifier at point.

## Recommended Setup

Bind the function to `M-.` in `js-ts-mode`:

```elisp
(add-hook 'js-ts-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") #'js-ts-defs-jump-to-definition)))
```

## Requirements

- Emacs 29.1 or later
- JavaScript tree-sitter grammar installed
