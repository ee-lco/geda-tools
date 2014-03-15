#include "libgedaruby.h"

VALUE ruby_object_class;

static VALUE ruby_object_alloc(VALUE class);
static void ruby_object_mark(void *obj);
static void ruby_object_free(void *obj);

void ruby_object_init(void)
{
    ruby_object_class = rb_define_class_under(ruby_geda_module, "Object", rb_cObject);
}

