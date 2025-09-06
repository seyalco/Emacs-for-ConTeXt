# context-lmtx-mode

`context-lmtx-mode` is a personal Emacs major mode designed for **editing and working with ConTeXt LMTX documents**.  
It is **automatically enabled only for files with the `.ctx` extension** and provides integration with multiple editing features.

---

## ✨ Features

- Automatic activation **only for `.ctx` files**
- Integration with [`polymode`](https://github.com/polymode/polymode) for multi-language editing
- Compatible with `company-mode` for auto-completion
- Supports `lua-mode` for embedded Lua scripts
- Supports `markdown-mode` for Markdown sections
- Includes an **`Optional/` folder** for extra tools and extensions:
  - **`template-tools.el`** → Manage, add, remove, and copy templates with simple keyboard shortcuts
  - Other optional modules that are loaded without interfering with the global `load-path`

---

## 📦 Installation

1. Copy the `context-lmtx-mode` folder into your personal Emacs packages directory:  

~/.emacs.d/Packages/context-lmtx-mode/

        

text

2. Ensure all dependencies are also present in the same directory:  

~/.emacs.d/Packages/polymode/

~/.emacs.d/Packages/company-mode/

~/.emacs.d/Packages/lua-mode/

~/.emacs.d/Packages/markdown-mode/

        

text

3. In your `init.el`, add:

```elisp
;; Path to personal packages
(let ((base "~/.emacs.d/Packages/"))
(dolist (path '("context-lmtx-mode"
"polymode"
"company-mode"
"lua-mode"
"markdown-mode"))
(add-to-list 'load-path (expand-file-name path base))))

;; Load main module
(require 'context-lmtx-mode)

;; Load optional extras (without adding to global load-path)
(load (expand-file-name
"context-lmtx-mode/Optional/myextras-loader.el"
"~/.emacs.d/Packages/"))

🖥️ Usage

    Open any .ctx file → context-lmtx-mode will start automatically.
    The mode enhances editing with syntax highlighting, code completion, and optional tools.

Template Management Shortcuts:

    C-c N → Add or remove a template
    C-c M → Copy a template from the saved list into the current directory

📂 Folder Structure

        

text
context-lmtx-mode/
│
├── context-lmtx-mode.el       # Main major mode file
├── Optional/                  # Extra optional tools
│   ├── myextras-loader.el     # Isolated loader for optional modules
│   └── template-tools.el      # Template management utilities
└── README.md                  # This file

---

## 📜 License

This package is a personal project and can be freely modified and used for personal or educational purposes.


