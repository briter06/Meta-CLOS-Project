sbcl --noinform ^
--load utils/miniclos/closless.lisp ^
--load utils/miniclos/miniclos.lisp ^
--eval "(in-package :closless)" ^
--load src/macros/class.lisp ^
--load src/functions/instance.lisp ^
%*