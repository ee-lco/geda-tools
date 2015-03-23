#include "libgedaruby.h"

VALUE ruby_geda_module;

void ruby_geda_init(void)
{
    ruby_geda_module = rb_define_module("GEDA");
}

