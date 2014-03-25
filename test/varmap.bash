#!/bin/bash
cmp -s ref/e3614a-2.VARMAP.sch <(ruby -I ../scripts -I ../src/.libs varmap.rb < ref/e3614a-2.EMBED.sch)
