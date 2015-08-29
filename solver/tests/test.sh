#!/bin/bash

tests=$(ls test_* | grep -v .pas)

err=0

for test in ${tests}; do
    echo
    echo ${test}
    ./${test}
    let err=${err}+$?
done

echo "Failed: ${err}"