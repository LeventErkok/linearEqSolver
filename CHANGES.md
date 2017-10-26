* Hackage: <http://hackage.haskell.org/package/linearEqSolver>
* GitHub:  <http://github.com/LeventErkok/linearEqSolver>

* Latest Hackage released version: 2.0

### Version 2.0, 2017-10-25

  * Use defaultSMTConfig exported from SBV
  * All-solution variants now take the max-number of solutions requested,
    following the corresponding changes in SBV itself. Thanks to Mitchell Rosen
    for reporting.

### Version 1.3, 2014-08-27

  * Use the Solver type from SBV directly for picking the solver,
    avoiding bit-rot.
  * Adjust SBV dependency to >= 3.1, to get proper access to
    Solver type

### Version 1.2, 2013-01-02

  * Allow both CVC4 and Z3 to be used as the SMT solver.
  * Adjust SBV dependency to >= 2.9, to get access to CVC4.

### Version 1.1, 2012-10-22

  * Add solvers over rationals, in addition to just integers.
  * Adjust SBV dependency to >= 2.7 as we depend on the new Real
    instance for the AlgReal type.

### Version 1.0, 2012-10-18

  * Initial release, contains solver for integer linear equations.
