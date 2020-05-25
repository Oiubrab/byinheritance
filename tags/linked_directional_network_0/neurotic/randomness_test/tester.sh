#!bin/bash

gfortran test_random.f95 -o testes
./testes
python plotter.py
rm testes
