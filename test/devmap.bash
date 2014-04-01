#!/bin/bash
cmp -s ref/e3614a-2.DEVMAP.sch <(ruby -I ../scripts -I ../src/.libs ../scripts/devmap/devmap.rb -P ../../../ps/e361xa/pcb/lib/dev/ < ref/e3614a-2.COND.sch)
