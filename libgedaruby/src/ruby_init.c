#include "libgedaruby.h"

void Init_libgedaruby()
{
    scm_init_guile();
    libgeda_init();

    ruby_geda_init();
    ruby_toplevel_init();
    ruby_page_init();
    ruby_object_init();
}

