#!/bin/sh

readonly BUILDAPP=/usr/bin/buildapp
readonly SOURCE_TREE=~/quicklisp/local-projects/

$BUILDAPP --output lass \
    --entry "BINARY-LASS:CMD-WRAPPER" \
    --load-system "binary-lass" \
    --asdf-tree $SOURCE_TREE \
    --compress-core
