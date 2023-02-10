This directory contains the nonlinear conjugate gradient method (NLCG)
for solving unconstrained multiobjective optimization problems described in

L. R. Lucambio Pérez and L. F. Prudente, [Nonlinear conjugate gradient 
methods for vector optimization](https://epubs.siam.org/doi/abs/10.1137/17M1126588), *SIAM Journal on Optimization* 28(3), 
pp. 2690-2720, 2018.

and

M. L. N. Gonçalves and L. F. Prudente, [On the extension of the 
Hager-Zhang conjugate gradient method for vector optimization](https://link.springer.com/article/10.1007/s10589-019-00146-1), 
*Computational Optimization and Applications* 76(3), pp. 889-916, 2020.

Last update: October of 2020

This folder also contains the third-party free codes which are used by NLCG: 
1) software Algencan 3.1.1;
    -  E. G. Birgin and J. M. Martı́nez, Practical augmented Lagrangian 
       methods for constrained optimization, SIAM, 2014.
    - https://www.ime.usp.br/~egbirgin/tango/
2) subroutines dcsrch and dcstep of Moré and Thuente.
    - J. J. Moré and D. J. Thuente, Line Search Algorithms with Guaranteed 
      Sufficient Decrease, ACM Trans. Math. Softw., 20 (1994), pp. 286–307.
    - http://ftp.mcs.anl.gov/pub/MINPACK-2/csrch/

File main.f90 contains the main program, where you can choose the CG 
parameter used. Modify myproblem routine to solve your own problem. 
Alternatively, set a test problem in main routine - see myproblem.f90.



Instructions:

The codes are written in Fortran 90. To compile and run NLCG, users need 
to install gcc.

1) Go to the main folder and type 

make

2) Run typing

./nlcgvo

and see the output in the screen.

