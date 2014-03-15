#ifndef LIBGEDARUBY_H
#define LIBGEDARUBY_H   1

#include <libgeda/libgeda.h>
#include <ruby.h>

extern VALUE ruby_geda_module;
void ruby_geda_init(void);

extern VALUE ruby_toplevel_class;
void ruby_toplevel_init(void);
TOPLEVEL *ruby_get_c_toplevel(void);
VALUE roby_toplevel_from_c(TOPLEVEL *toplevel);

extern VALUE ruby_page_class;
void ruby_page_init(void);
VALUE ruby_page_from_c(PAGE *page);

extern VALUE ruby_object_class;
void ruby_object_init(void);
VALUE ruby_object_from_c(OBJECT *object);

#endif

