#!/bin/bash
cmp -s ref/e3614a-2.COND.sch <(ruby -I ../scripts -I ../src/.libs ../scripts/cond/cond.rb < ref/e3614a-2.EMBED.sch)
