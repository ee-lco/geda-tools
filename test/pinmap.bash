#!/bin/bash
cmp -s ref/e3614a-2.PINMAP.sch <(ruby -I ../scripts -I ../src/.libs pinmap.rb < ref/e3614a-2.DEVMAP.sch)
