#!/bin/bash
cmp -s ref/e3614a-2.DEVMAP.sch <(ruby -I ../scripts -I ../src/.libs devmap.rb < ref/e3614a-2.VARMAP.sch)
