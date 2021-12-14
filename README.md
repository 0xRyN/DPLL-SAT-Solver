# OCAML Sat Solver

## An ultra fast OCAML Sat Solver

Benchmark :

time ./dpll sudoku-9x9-god.cnf (9106 lines)
SAT (Solution)
...

0.65s user 0.14s system 99 cpu 0.787 total

## Usage

You need to have OCAML installed in your machine.

1. Clone this repository

2. Put your DIMACS Format file in the same repertory

3. `make`

4. `./dpll "yourfile.cnf"`

## Made for a school project @ Université de Paris
