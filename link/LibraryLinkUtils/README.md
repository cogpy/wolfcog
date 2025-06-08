# LibraryLinkUtils Integration

This directory contains utilities for connecting the Wolfram Engine to Guile/C++ codebases and exposing AtomSpace memory graphs to symbolic kernels.

## Purpose

- Connect Wolfram Engine to C/C++ code
- Expose AtomSpace memory structures 
- Bridge symbolic kernels with compiled libraries
- Enable runtime symbolic evaluation

## Files

- `atomspace-bindings.h` - C header bindings for AtomSpace
- `wolfram-bridge.cpp` - Bridge implementation
- `symbolic-memory.wl` - Wolfram-side memory interface

## Usage

Include these utilities to enable symbolic kernel access to compiled memory structures and external libraries.