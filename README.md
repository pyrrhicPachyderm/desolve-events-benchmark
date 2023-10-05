# deSolve Events Benchmark

R's `deSolve` package comes with an events system, for handling instantaneous changes to state variables.
However, I have heard that it is very slow.
Thus, this is an empirical test, comparing the speed of using the events system to simply halting the solver at the appropriate time, adjusting the state variables manually, and restarting the solver with the new set of initial values.
