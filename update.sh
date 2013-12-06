#!/bin/sh


# byte-compile files
emacs --no-init-file --batch -f batch-byte-compile *.el
