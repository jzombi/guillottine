#!/bin/bash

rm -r doc/html
rm -r doc/pdf
mkdir doc/html
mkdir doc/pdf

#Don't generate pasdoc for GUI yet.
#files="""
#*.pas
#solver/*.pas
#solver/tests/test.pas
#"""

files="""
solver/*.pas
solver/tests/test.pas
"""


pasdoc --files $files --output doc/html --format html
pasdoc --files $files --output doc/pdf --format latex

tmp=$(pwd)
cd doc/pdf
pdflatex docs.tex
pdflatex docs.tex
cd ${tmp}