#!/bin/bash

if [ -z "$EDITOR" ]
then
    echo 'set my_editor="vim"'
else
    echo 'set my_editor="$EDITOR"'
fi

