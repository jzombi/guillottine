#!/bin/bash

rm -r doc/*
mkdir doc/html
mkdir doc/pdf

files="""
*.pas
solver/*.pas
solver/tests/test.pas
"""

pasdoc --files $files --output doc/html --format html
pasdoc --files $files --output doc/pdf --format latex

tmp=$(pwd)
cd doc/pdf
latex docs.tex
cd ${tmp}