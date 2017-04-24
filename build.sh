#!/bin/sh

sbcl \
    --no-userinit \
    --load build.lisp \
    --name ninja-sphere \
    --startup "ninja-sphere:run" \
    --non-interactive
