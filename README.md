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

## API

This package exposes several functions for programmatic use:

### `js-ts-defs-build-scope`

```elisp
(js-ts-defs-build-scope root-node)
```

Builds a scope structure from a tree-sitter root node. Returns a nested scope object containing variable definitions and their positions.

**Parameters:**
- `root-node`: A tree-sitter root node from `(treesit-buffer-root-node)`

**Returns:** A scope object (see structure below)

### `js-ts-defs-find-definition`

```elisp
(js-ts-defs-find-definition scope identifier position)
```

Finds the definition of an identifier within a scope structure.

**Parameters:**
- `scope`: A scope object returned by `js-ts-defs-build-scope`
- `identifier`: String name of the identifier to find
- `position`: Buffer position where the identifier is used

**Returns:** Buffer position of the definition, or `nil` if not found

### Scope Object Structure

The scope object is a property list with the following structure:

```elisp
(:type "program"           ; Scope type: "program", "function", "block", "for"
 :start 1                  ; Starting buffer position
 :end 100                  ; Ending buffer position
 :variables #<hash-table>  ; Hash table mapping identifier names to positions
 :children (...)           ; List of child scope objects
 :is-arrow t)              ; Optional: present for arrow functions
```

**Scope Types:**
- `"program"`: Top-level/global scope
- `"function"`: Function scope (including arrow functions)
- `"block"`: Block scope (for let/const declarations)
- `"for"`: For-loop scope (for let/const in loop initializers)

**Example Usage:**

```elisp
;; Build scope from current buffer
(let* ((root-node (treesit-buffer-root-node))
       (scope (js-ts-defs-build-scope root-node)))

  ;; Find definition of "myVar" at position 150
  (js-ts-defs-find-definition scope "myVar" 150))
```

## Requirements

- Emacs 29.1 or later
- JavaScript tree-sitter grammar installed
