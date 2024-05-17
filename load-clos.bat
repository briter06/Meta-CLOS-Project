sbcl --noinform ^
--load src/tests/loader.lisp ^
--eval "(load \"~/quicklisp/setup.lisp\")" ^
--eval "(ql:quickload \"closer-mop\")" ^
%*