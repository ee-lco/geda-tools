#include "libgedaruby.h"

static TOPLEVEL *toplevel;

TOPLEVEL *ruby_get_c_toplevel(void)
{

    if (!toplevel) {
    }

    return toplevel;
}

void ruby_toplevel_init(void)
{
    VALUE argv0;
    argv0 = rb_gv_get("$0");

    toplevel = s_toplevel_new();

    g_rc_parse(toplevel, StringValueCStr(argv0), "grubyrc", NULL);
    i_vars_libgeda_set(toplevel);
}

