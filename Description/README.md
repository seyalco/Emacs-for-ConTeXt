# Emacs for ConTeXt – Documentation Set

This directory contains the **documentation sources** for the `context-lmtx-mode` package,  
written entirely in **ConTeXt** format.

## Main File

The main entry point for compilation is:

Emacs-for-ConTeXt.ctx

This file collects all individual documentation chapters (e.g., context-lmtx-mode.el,

session-saving.el, tools, etc.) and produces a single, complete PDF manual.
How to Compile

Make sure you have a working ConTeXt LMTX installation.

In the terminal, run:

bash

context Emacs-for-ConTeXt.ctx

ConTeXt will parse the .ctx file and generate a PDF containing the entire documentation.
Version Tested

This manual was compiled and tested using:

mtx-context | current version: 2025.07.08 17:48
Output

The compilation produces a PDF file in the same directory:

Emacs-for-ConTeXt.pdf

Open it in your preferred PDF viewer to read the complete documentation.
Notes

    All .ctx files in this directory are written in pure ConTeXt,without Markdown formatting.
    Each code-related feature is documented in a separate chapter for clarity.
    You only need to compile the main .ctx file; it will automatically include all others.
