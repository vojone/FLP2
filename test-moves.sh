#!/bin/bash

# flp23-log
#
# Testing script for Rubik's cube solver
#
# Author: Vojtěch Dvořák (xdvora3o)

shopt -s extglob

TEST_DIR="tests/moves/*" # Directory with tests

for CASE in $TEST_DIR; do
    echo -n "$CASE: "

    INPUT="$CASE/$(ls "$CASE" | grep ".*.in")" # Text file with initial cube
    TARGET="$CASE/$(ls "$CASE" | grep ".*.out")" # Text file with expected cube
    SCRIPT="$CASE/$(ls "$CASE" | grep ".*.pl")" # Prolog script for swipl

    if [ ! -f "$INPUT" ] || [ ! -f "$TARGET" ] || [ ! -f "$SCRIPT" ]; then
        echo "Incomplete test case!"
    else
        RESULT=$(swipl -s "$SCRIPT" <"$INPUT" 2>/dev/null)
        DIFFS=$(diff -Z "$TARGET" <(echo "$RESULT"))

        if [ -z "$DIFFS" ]; then
            printf "OK"
        else
            printf "FAIL\nExpected:\n%s\nGot:\n%s\n" "$(cat "$TARGET")" "$RESULT"
        fi
    fi

    echo
done

