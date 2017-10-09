#!/bin/bash

find . -maxdepth 1 -type f -name '*.scm' | while read test; do
    echo -e "\033[1mRunning tests in ${test}\033[0m"
    racket $test
    echo
done

