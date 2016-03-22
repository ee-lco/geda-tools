#ifndef LIBGEDARUBY_H
#define LIBGEDARUBY_H   1

#include <libgeda/libgeda.h>
#include <ruby.h>

extern VALUE ruby_geda_module;
void ruby_geda_init(void);

void ruby_toplevel_init(void);
TOPLEVEL *ruby_get_c_toplevel(void);

extern VALUE ruby_page_class;
void ruby_page_init(void);
VALUE ruby_page_from_c(PAGE *page);
PAGE *ruby_page_to_c(VALUE page);

extern VALUE ruby_object_class;
void ruby_object_init(void);
VALUE ruby_object_from_c(OBJECT *object);
OBJECT *ruby_object_to_c(VALUE object);

extern VALUE ruby_line_class;
extern VALUE ruby_net_class;
extern VALUE ruby_bus_class;
extern VALUE ruby_pin_class;
extern VALUE ruby_box_class;
extern VALUE ruby_circle_class;
extern VALUE ruby_arc_class;
extern VALUE ruby_path_class;
extern VALUE ruby_picture_class;
extern VALUE ruby_text_class;
extern VALUE ruby_attrib_class;

#endif

