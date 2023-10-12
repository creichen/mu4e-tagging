#! /bin/bash
emacs -batch -l ert -l mu4e-tagging-test.el -f ert-run-tests-batch-and-exit
