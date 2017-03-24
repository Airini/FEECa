#! /bin/bash

./../../dist/build/bench/bench &> bench.dat
python2.7 plot.py
