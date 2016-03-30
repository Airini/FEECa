#! /bin/bash

./bench &> bench.dat
python2.7 plot.py
