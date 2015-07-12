#!/bin/sh
set -ue
echo "G (F p)" | ./ltl 2>/dev/null >test1.dot
echo "G (p -> F q)" | ./ltl 2>/dev/null >test2.dot
echo "(~p) U (X q)" | ./ltl 2>/dev/null >test3.dot
echo "G ((F p) -> q)" | ./ltl 2>/dev/null >test4.dot
dot -Tpng test1.dot > test1.png
dot -Tpng test2.dot > test2.png
dot -Tpng test3.dot > test3.png
dot -Tpng test4.dot > test4.png
