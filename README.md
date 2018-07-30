cl-lr
===========

A library written in Common Lisp to estimate the paramters of a logistic regression model.


## Prerequisite

Prepare `liblbfgs.so`, which is built by libLBFGS.

Add the path to a directory containing `liblbfgs.so` to `LD_LIBRARY_PATH`.
```
export LD_LIBRARY_PATH=path/to/liblbfgs/dir/:$LD_LIBRARY_PATH
```


## Usage

See `./t/scenario/simple-calculation.lisp`.


## License

MIT
