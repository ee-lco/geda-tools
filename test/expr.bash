#!/bin/bash
cmp -s ref/e3614a-2.EMBED.sch <(ruby -I ../scripts/expr -I ../scripts -I ../src/.libs ../scripts/expr/expr.rb < ref/e3614a-2.EXPR.sch)
