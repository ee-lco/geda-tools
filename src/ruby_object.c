#include "libgedaruby.h"

VALUE ruby_object_class;

static VALUE ruby_object_alloc(VALUE class);
static void ruby_object_mark(void *obj);
static void ruby_object_free(void *obj);
static VALUE ruby_object_attribs_get(VALUE self);
static VALUE ruby_object_attach_attribs(int argc, VALUE argv[], VALUE self);
static VALUE ruby_object_detach_attribs(int argc, VALUE argv[], VALUE self);
static VALUE ruby_object_connections_get(VALUE self);
static VALUE ruby_object_copy(VALUE self);
static VALUE ruby_object_component_get(VALUE self);
static VALUE ruby_object_page_get(VALUE self);

VALUE ruby_vector_class;
static VALUE ruby_vector_initialize(int argc, VALUE argv[], VALUE self);
static VALUE ruby_vector_x_get(VALUE self);
static VALUE ruby_vector_x_set(VALUE self, VALUE x);
static VALUE ruby_vector_y_get(VALUE self);
static VALUE ruby_vector_y_set(VALUE self, VALUE y);

VALUE ruby_line_class;

VALUE ruby_net_class;

VALUE ruby_bus_class;

VALUE ruby_pin_class;

VALUE ruby_box_class;

VALUE ruby_circle_class;

VALUE ruby_arc_class;

VALUE ruby_path_class;

VALUE ruby_picture_class;

VALUE ruby_text_class;
static VALUE ruby_text_initialize(int argc, VALUE argv[], VALUE self);
static VALUE ruby_text_set(int argc, VALUE argv[], VALUE self);
static VALUE ruby_text_anchor_get(VALUE self);
static VALUE ruby_text_anchor_set(int argc, VALUE argv[], VALUE self);
static VALUE ruby_text_alignment_get(VALUE self);
static VALUE ruby_text_alignment_set(VALUE self, VALUE alignment);
static VALUE ruby_text_angle_get(VALUE self);
static VALUE ruby_text_angle_set(VALUE self, VALUE angle);
static VALUE ruby_text_string_get(VALUE self);
static VALUE ruby_text_string_set(VALUE self, VALUE string);
static VALUE ruby_text_size_get(VALUE self);
static VALUE ruby_text_size_set(VALUE self, VALUE size);
static VALUE ruby_text_visible_get(VALUE self);
static VALUE ruby_text_visible_set(VALUE self, VALUE visible);
static VALUE ruby_text_attrib_mode_get(VALUE self);
static VALUE ruby_text_attrib_mode_set(VALUE self, VALUE show);

VALUE ruby_attrib_class;
static VALUE ruby_attrib_name_get(VALUE self);
static VALUE ruby_attrib_value_get(VALUE self);
static VALUE ruby_attrib_value_set(VALUE self, VALUE value);
static VALUE ruby_attrib_attachment_get(VALUE self);
static VALUE ruby_attrib_attachment_set(VALUE self, VALUE target);
static VALUE ruby_attrib_detach(VALUE self);
static VALUE ruby_attrib_inherited_get(VALUE self);

VALUE ruby_component_class;
static VALUE ruby_component_set(VALUE self, VALUE position, VALUE angle, VALUE mirror, VALUE locked);
static VALUE ruby_component_basename_get(VALUE self);
static VALUE ruby_component_position_get(VALUE self);
static VALUE ruby_component_position_set(int argc, VALUE argv[], VALUE self);
static VALUE ruby_component_angle_get(VALUE self);
static VALUE ruby_component_angle_set(VALUE self, VALUE angle);
static VALUE ruby_component_mirror_get(VALUE self);
static VALUE ruby_component_mirror_set(VALUE self, VALUE mirror);
static VALUE ruby_component_locked_get(VALUE self);
static VALUE ruby_component_locked_set(VALUE self, VALUE locked);
static VALUE ruby_component_contents_get(VALUE self);
static VALUE ruby_component_append(int argc, VALUE argv[], VALUE self);
static VALUE ruby_component_remove(int argc, VALUE argv[] , VALUE self);


VALUE ruby_object_from_c(OBJECT *object)
{
    VALUE class;

    switch (object->type) {
    case OBJ_LINE:
        class = ruby_line_class;
        break;
    case OBJ_NET:
        class = ruby_net_class;
        break;
    case OBJ_BUS:
        class = ruby_bus_class;
        break;
    case OBJ_PIN:
        class = ruby_pin_class;
        break;
    case OBJ_BOX:
        class = ruby_box_class;
        break;
    case OBJ_CIRCLE:
        class = ruby_circle_class;
        break;
    case OBJ_ARC:
        class = ruby_arc_class;
        break;
    case OBJ_PATH:
        class = ruby_path_class;
        break;
    case OBJ_PICTURE:
        class = ruby_picture_class;
        break;
    case OBJ_TEXT:
        if (o_attrib_get_name_value(object, NULL, NULL)) {
            class = ruby_attrib_class;
        } else {
            class = ruby_text_class;
        }
        break;
    case OBJ_PLACEHOLDER:
    case OBJ_COMPLEX:
        class = ruby_component_class;
        break;

    default:
fprintf(stderr, "%s: unknown object type %d\n", __func__, (int)object->type);
        ///@todo
        break;
    }

    return Data_Wrap_Struct(class, ruby_object_mark, ruby_object_free, object);
}

OBJECT *ruby_object_to_c(VALUE object)
{
    OBJECT *obj;

    Data_Get_Struct(object, OBJECT, obj);

    return obj;
}

static void ruby_object_mark(void *obj)
{
    ///@todo
}

static void ruby_object_free(void *obj)
{
    ///@todo
}

static VALUE ruby_object_attribs_get(VALUE self)
{
    OBJECT *object;
    VALUE attribs;

    object = ruby_object_to_c(self);

    attribs = rb_ary_new();
    GList *attrib_list = object->attribs;
    while (attrib_list != NULL) {
        rb_ary_push(attribs, ruby_object_from_c(attrib_list->data));
        attrib_list = g_list_next(attrib_list);
    }

    return attribs;
}

static VALUE ruby_object_attach_attribs(int argc, VALUE argv[], VALUE self)
{
    int i;

    for (i = 0; i < argc; i++) {
        rb_funcall(argv[i], rb_intern("attach!"), 1, self);
    }
}

static VALUE ruby_object_detach_attribs(int argc, VALUE argv[], VALUE self)
{
    int i;

    for (i = 0; i < argc; i++) {
        rb_funcall(argv[i], rb_intern("detach!"), 1, self);
    }
}

static VALUE ruby_object_connections_get(VALUE self)
{
    TOPLEVEL *toplevel;
    OBJECT *object;
    PAGE *page;
    VALUE connections;

    toplevel = ruby_get_c_toplevel();
    object = ruby_object_to_c(self);
    page = o_get_page(toplevel, object);

    if (page == NULL) {
        ///@todo
    }

    connections = rb_ary_new();
    GList *connection_list = s_conn_return_others(NULL, object);
    while (connection_list != NULL) {
        rb_ary_push(connections, ruby_object_from_c(connection_list->data));
        connection_list = g_list_next(connection_list);
    }

    return connections;
}

static VALUE ruby_object_copy(VALUE self)
{
    TOPLEVEL *toplevel;
    OBJECT *object;
    OBJECT *copy;

    toplevel = ruby_get_c_toplevel();
    object = ruby_object_to_c(self);
    copy = o_object_copy(toplevel, object);

    return ruby_object_from_(copy);
}

static VALUE ruby_object_component_get(VALUE self)
{
    TOPLEVEL *toplevel;
    OBJECT *object;
    OBJECT *parent;

    toplevel = ruby_get_c_toplevel();
    object = ruby_object_to_c(self);
    parent = o_get_parent(toplevel, object);

    if (!parent) {
        return Qnil;
    }
    return ruby_object_from_c(parent);
}

static VALUE ruby_object_page_get(VALUE self)
{
    TOPLEVEL *toplevel;
    OBJECT *object;
    PAGE *page;

    toplevel = ruby_get_c_toplevel();
    object = ruby_object_to_c(self);
    page = o_get_page(toplevel, object);

    if (!page) {
        return Qnil;
    }
    return ruby_page_from_c(page);
}

static VALUE ruby_vector_initialize(int argc, VALUE argv[], VALUE self)
{
    VALUE vector;
    VALUE x, y;

    if (argc == 1) {
        rb_scan_args(argc, argv, "10", &vector);
        switch (TYPE(vector)) {
        case T_OBJECT:
            x = rb_iv_get(vector, "@x");
            y = rb_iv_get(vector, "@y");
            break;
        case T_ARRAY:
            x = rb_ary_entry(vector, 0);
            y = rb_ary_entry(vector, 1);
            break;
        default:
            ///@todo
            break;
        }
    } else {
        rb_scan_args(argc, argv, "20", &x, &y);
    }

    if (TYPE(x) != T_FIXNUM || TYPE(y) != T_FIXNUM) {
        ///@todo
    }

    ruby_vector_x_set(self, x);
    ruby_vector_y_set(self, y);

    return self;
}

static VALUE ruby_vector_x_get(VALUE self)
{
    return rb_iv_get(self, "@x");
}

static VALUE ruby_vector_x_set(VALUE self, VALUE x)
{
    return rb_iv_set(self, "@x", x);
}

static VALUE ruby_vector_y_get(VALUE self)
{
    return rb_iv_get(self, "@y");
}

static VALUE ruby_vector_y_set(VALUE self, VALUE y)
{
    return rb_iv_set(self, "@y", y);
}

static VALUE ruby_text_initialize(int argc, VALUE argv[], VALUE self)
{
    return ruby_text_set(argc, argv, self);
}

static VALUE ruby_text_set(int argc, VALUE argv[], VALUE self)
{
    OBJECT *object;
    VALUE anchor;
    VALUE alignment;
    VALUE angle;
    VALUE string;
    VALUE size;
    VALUE visible;
    VALUE show;
    VALUE color;

    object = ruby_object_to_c(self);

    rb_scan_args(argc, argv, "71", &anchor, &alignment, &angle, &string, &size, &visible, &show, &color);

    ruby_text_anchor_set(1, &anchor, self);
    ruby_text_alignment_set(self, alignment);
    ruby_text_angle_set(self, angle);
    ruby_text_string_set(self, string);
    ruby_text_size_set(self, size);
    ruby_text_visible_set(self, visible);
    ruby_text_attrib_mode_set(self, show);
    //if (!NIL_P(color)) {
        //ruby_object_color_set(self, color);
    //}
}

static VALUE ruby_text_anchor_get(VALUE self)
{
    OBJECT *object;
    VALUE x, y;

    object = ruby_object_to_c(self);
    x = INT2FIX(object->text->x);
    y = INT2FIX(object->text->y);

    return rb_funcall(ruby_vector_class, rb_intern("new"), 2, x, y);
}

static VALUE ruby_text_anchor_set(int argc, VALUE argv[], VALUE self)
{
    OBJECT *object;
    VALUE anchor;

    object = ruby_object_to_c(self);
    anchor = rb_funcall2(ruby_vector_class, rb_intern("new"), argc, argv);

    object->text->x = NUM2INT(rb_funcall(anchor, rb_intern("x"), 0));
    object->text->y = NUM2INT(rb_funcall(anchor, rb_intern("y"), 0));

    return self;
}

static VALUE ruby_text_alignment_get(VALUE self)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    switch (object->text->alignment) {
    case LOWER_LEFT:
        return ID2SYM(rb_intern(":lower_left"));
    case MIDDLE_LEFT:
        return ID2SYM(rb_intern(":middle_left"));
    case UPPER_LEFT:
        return ID2SYM(rb_intern(":upper_left"));
    case LOWER_MIDDLE:
        return ID2SYM(rb_intern(":lower_middle"));
    case MIDDLE_MIDDLE:
        return ID2SYM(rb_intern(":middle_middle"));
    case UPPER_MIDDLE:
        return ID2SYM(rb_intern(":upper_middle"));
    case LOWER_RIGHT:
        return ID2SYM(rb_intern(":lower_right"));
    case MIDDLE_RIGHT:
        return ID2SYM(rb_intern(":middle_right"));
    case UPPER_RIGHT:
        return ID2SYM(rb_intern(":upper_right"));
    default:
        ///@todo
        return Qnil;
    }
}

static VALUE ruby_text_alignment_set(VALUE self, VALUE alignment)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    Check_Type(alignment, T_SYMBOL);

    if (SYM2ID(alignment) == rb_intern(":lower_left")) {
        object->text->alignment = LOWER_LEFT;
    } else if (SYM2ID(alignment) == rb_intern(":middle_left")) {
        object->text->alignment = MIDDLE_LEFT;
    } else if (SYM2ID(alignment) == rb_intern(":upper_left")) {
        object->text->alignment = UPPER_LEFT;
    } else if (SYM2ID(alignment) == rb_intern(":lower_middle")) {
        object->text->alignment = LOWER_MIDDLE;
    } else if (SYM2ID(alignment) == rb_intern(":middle_middle")) {
        object->text->alignment = MIDDLE_MIDDLE;
    } else if (SYM2ID(alignment) == rb_intern(":upper_middle")) {
        object->text->alignment = UPPER_MIDDLE;
    } else if (SYM2ID(alignment) == rb_intern(":lower_right")) {
        object->text->alignment = LOWER_RIGHT;
    } else if (SYM2ID(alignment) == rb_intern(":middle_right")) {
        object->text->alignment = MIDDLE_RIGHT;
    } else if (SYM2ID(alignment) == rb_intern(":upper_right")) {
        object->text->alignment = UPPER_RIGHT;
    } else {
        ///@todo
    }

    return self;
}

static VALUE ruby_text_angle_get(VALUE self)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    return INT2NUM(object->text->angle);
}

static VALUE ruby_text_angle_set(VALUE self, VALUE angle)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    Check_Type(angle, T_FIXNUM);
    switch (NUM2INT(angle)) {
    case 0:
    case 90:
    case 180:
    case 270:
        break;
    default:
        ///@todo
        break;
    }

    object->text->angle = NUM2INT(angle);

    return self;
}

static VALUE ruby_text_string_get(VALUE self)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    return rb_str_new2(object->text->string);
}

static VALUE ruby_text_string_set(VALUE self, VALUE string)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    if (object->text->string != NULL) {
        g_free(object->text->string);
    }

    object->text->string = g_strdup(StringValueCStr(string));

    return self;
}

static VALUE ruby_text_size_get(VALUE self)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    return INT2NUM(object->text->size);
}

static VALUE ruby_text_size_set(VALUE self, VALUE size)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    Check_Type(size, T_FIXNUM);

    object->text->size = NUM2INT(size);

    return self;
}

static VALUE ruby_text_visible_get(VALUE self)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    return (object->visibility == INVISIBLE) ? Qfalse : Qtrue;
}

static VALUE ruby_text_visible_set(VALUE self, VALUE visible)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    object->visibility = RTEST(visible) ? VISIBLE : INVISIBLE;

    return self;
}

static VALUE ruby_text_attrib_mode_get(VALUE self)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    switch (object->show_name_value) {
    case SHOW_NAME:
        return ID2SYM(rb_intern(":name"));
    case SHOW_VALUE:
        return ID2SYM(rb_intern(":value"));
    case SHOW_NAME_VALUE:
        return ID2SYM(rb_intern(":both"));
    default:
        ///@todo
        return Qnil;
    }
}

static VALUE ruby_text_attrib_mode_set(VALUE self, VALUE show)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    Check_Type(show, T_SYMBOL);

    if (SYM2ID(show) == rb_intern(":name")) {
        object->show_name_value = SHOW_NAME;
    } else if (SYM2ID(show) == rb_intern(":value")) {
        object->show_name_value = SHOW_VALUE;
    } else if (SYM2ID(show) == rb_intern(":both")) {
        object->show_name_value = SHOW_NAME_VALUE;
    } else {
        ///@todo
    }

    return self;
}

static VALUE ruby_attrib_name_get(VALUE self)
{
    OBJECT *object;
    gchar *name;

    object = ruby_object_to_c(self);
    if (!o_attrib_get_name_value(object, &name, NULL)) {
        ///@todo
    }
    return rb_str_new2(name);
}

static VALUE ruby_attrib_value_get(VALUE self)
{
    OBJECT *object;
    gchar *value;

    object = ruby_object_to_c(self);
    if (!o_attrib_get_name_value(object, NULL, &value)) {
        ///@todo
    }
    return rb_str_new2(value);
}

static VALUE ruby_attrib_value_set(VALUE self, VALUE value)
{
    OBJECT *object;
    gchar *name;
    gchar *string;

    object = ruby_object_to_c(self);
    if (!o_attrib_get_name_value(object, &name, NULL)) {
        ///@todo
    }

    string = g_strdup_printf("%s=%s", name, StringValueCStr(value));

    return ruby_text_string_set(self, rb_str_new2(string));
}

static VALUE ruby_attrib_attachment_get(VALUE self)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    if (!object->attached_to) {
        return Qnil;
    }

    return ruby_object_from_c(object->attached_to);
}

static VALUE ruby_attrib_attachment_set(VALUE self, VALUE target)
{
    TOPLEVEL *toplevel;
    OBJECT *object;
    OBJECT *tgt;

    toplevel = ruby_get_c_toplevel();
    object = ruby_object_to_c(self);
    tgt = ruby_object_to_c(target);

    ///@todo

    o_attrib_attach(toplevel, object, tgt, TRUE);

    return self;
}

static VALUE ruby_attrib_detach(VALUE self)
{
    TOPLEVEL *toplevel;
    OBJECT *object;

    toplevel = ruby_get_c_toplevel();
    object = ruby_object_to_c(self);

    ///@todo

    o_attrib_remove(toplevel, &object->attached_to->attribs, object);

    return self;
}

static VALUE ruby_attrib_inherited_get(VALUE self)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    if (!object->attached_to && object->parent) {
        return Qtrue;
    }

    return Qfalse;
}

static VALUE ruby_component_set(VALUE self, VALUE position, VALUE angle, VALUE mirror, VALUE locked)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    ruby_component_position_set(1, &position, self);
    ruby_component_angle_set(self, angle);
    ruby_component_mirror_set(self, mirror);
    ruby_component_locked_set(self, locked);
}

static VALUE ruby_component_basename_get(VALUE self)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    return rb_str_new2(object->complex_basename);
}

static VALUE ruby_component_position_get(VALUE self)
{
    OBJECT *object;
    VALUE x, y;

    object = ruby_object_to_c(self);
    x = INT2FIX(object->complex->x);
    y = INT2FIX(object->complex->y);

    return rb_funcall(ruby_vector_class, rb_intern("new"), 2, x, y);
}

static VALUE ruby_component_position_set(int argc, VALUE argv[], VALUE self)
{
    OBJECT *object;
    VALUE position;

    object = ruby_object_to_c(self);
    position = rb_funcall2(ruby_vector_class, rb_intern("new"), argc, argv);

    object->complex->x = NUM2INT(rb_funcall(position, rb_intern("x"), 0));
    object->complex->y = NUM2INT(rb_funcall(position, rb_intern("y"), 0));

    return self;
}

static VALUE ruby_component_angle_get(VALUE self)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    return INT2NUM(object->complex->angle);
}

static VALUE ruby_component_angle_set(VALUE self, VALUE angle)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    Check_Type(angle, T_FIXNUM);
    switch (NUM2INT(angle)) {
    case 0:
    case 90:
    case 180:
    case 270:
        break;
    default:
        ///@todo
        break;
    }

    object->complex->angle = NUM2INT(angle);

    return self;
}

static VALUE ruby_component_mirror_get(VALUE self)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    return (object->complex->mirror) ? Qtrue : Qfalse;
}

static VALUE ruby_component_mirror_set(VALUE self, VALUE mirror)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    object->complex->mirror = RTEST(mirror) ? 1 : 0;

    return self;
}

static VALUE ruby_component_locked_get(VALUE self)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    return (object->selectable) ? Qtrue : Qfalse;
}

static VALUE ruby_component_locked_set(VALUE self, VALUE locked)
{
    OBJECT *object;

    object = ruby_object_to_c(self);

    object->selectable = RTEST(locked) ? 1 : 0;

    return self;
}

static VALUE ruby_component_contents_get(VALUE self)
{
    OBJECT *object;
    VALUE objects;

    object = ruby_object_to_c(self);

    objects = rb_ary_new();
    GList *object_list = object->complex->prim_objs;
    while (object_list != NULL) {
        rb_ary_push(objects, ruby_object_from_c(object_list->data));
        object_list = g_list_next(object_list);
    }

    return objects;
}

static VALUE ruby_component_append(int argc, VALUE argv[], VALUE self)
{
    OBJECT *object;
    OBJECT *child;
    int i;

    object = ruby_object_to_c(self);

    for (i = 0; i < argc; i++) {
        child = ruby_object_to_c(argv[i]);

        ///@todo

        g_list_append(object->complex->prim_objs, child);
        child->parent = object;
    }
    ///@todo

    return Qnil;
}

static VALUE ruby_component_remove(int argc, VALUE argv[] , VALUE self)
{
    OBJECT *object;
    OBJECT *child;
    int i;

    object = ruby_object_to_c(self);

    for (i = 0; i < argc; i++) {
        child = ruby_object_to_c(argv[i]);

        ///@todo

        g_list_remove_all(object->complex->prim_objs, child);
        child->parent = NULL;
    }
    ///@todo

    return Qnil;
}

void ruby_object_init(void)
{
    ruby_object_class = rb_define_class_under(ruby_geda_module, "Object", rb_cObject);
    //rb_define_alloc_func(ruby_object_class, ruby_object_alloc);
    rb_define_method(ruby_object_class, "attribs", ruby_object_attribs_get, 0);
    rb_define_method(ruby_object_class, "attach_attribs!", ruby_object_attach_attribs, -1);
    rb_define_method(ruby_object_class, "detach_attribs!", ruby_object_detach_attribs, -1);
    rb_define_method(ruby_object_class, "connections", ruby_object_connections_get, 0);
    rb_define_method(ruby_object_class, "copy", ruby_object_copy, 0);
    rb_define_method(ruby_object_class, "component", ruby_object_component_get, 0);
    rb_define_method(ruby_object_class, "page", ruby_object_page_get, 0);

    ruby_vector_class = rb_define_class_under(ruby_geda_module, "Vector", rb_cObject);
    rb_define_method(ruby_vector_class, "initialize", ruby_vector_initialize, -1);
    rb_define_method(ruby_vector_class, "x", ruby_vector_x_get, 0);
    rb_define_method(ruby_vector_class, "x=", ruby_vector_x_get, 1);
    rb_define_method(ruby_vector_class, "y", ruby_vector_y_get, 0);
    rb_define_method(ruby_vector_class, "y=", ruby_vector_y_get, 1);

    ruby_line_class = rb_define_class_under(ruby_geda_module, "Line", ruby_object_class);

    ruby_net_class = rb_define_class_under(ruby_geda_module, "Net", ruby_object_class);

    ruby_bus_class = rb_define_class_under(ruby_geda_module, "Bus", ruby_object_class);

    ruby_pin_class = rb_define_class_under(ruby_geda_module, "Pin", ruby_object_class);

    ruby_box_class = rb_define_class_under(ruby_geda_module, "Box", ruby_object_class);

    ruby_circle_class = rb_define_class_under(ruby_geda_module, "Circle", ruby_object_class);

    ruby_arc_class = rb_define_class_under(ruby_geda_module, "Arc", ruby_object_class);

    ruby_path_class = rb_define_class_under(ruby_geda_module, "Path", ruby_object_class);

    ruby_picture_class = rb_define_class_under(ruby_geda_module, "Picture", ruby_object_class);

    ruby_text_class = rb_define_class_under(ruby_geda_module, "Text", ruby_object_class);
    rb_define_method(ruby_text_class, "set", ruby_text_set, -1);
    rb_define_method(ruby_text_class, "anchor", ruby_text_anchor_get, 0);
    rb_define_method(ruby_text_class, "anchor=", ruby_text_anchor_set, -1);
    rb_define_method(ruby_text_class, "alignment", ruby_text_alignment_get, 0);
    rb_define_method(ruby_text_class, "alignment=", ruby_text_alignment_set, 1);
    rb_define_method(ruby_text_class, "angle", ruby_text_angle_get, 0);
    rb_define_method(ruby_text_class, "angle=", ruby_text_angle_set, 1);
    rb_define_method(ruby_text_class, "string", ruby_text_string_get, 0);
    rb_define_method(ruby_text_class, "string=", ruby_text_string_set, 1);
    rb_define_method(ruby_text_class, "size", ruby_text_size_get, 0);
    rb_define_method(ruby_text_class, "visible?", ruby_text_visible_get, 0);
    rb_define_method(ruby_text_class, "visible=", ruby_text_visible_get, 1);
    rb_define_method(ruby_text_class, "attrib_mode", ruby_text_attrib_mode_get, 0);
    rb_define_method(ruby_text_class, "attrib_mode=", ruby_text_attrib_mode_get, 1);

    ruby_attrib_class = rb_define_class_under(ruby_geda_module, "Attrib", ruby_text_class);
    rb_define_method(ruby_attrib_class, "name", ruby_attrib_name_get, 0);
    rb_define_method(ruby_attrib_class, "value", ruby_attrib_value_get, 0);
    rb_define_method(ruby_attrib_class, "value=", ruby_attrib_value_set, 1);
    rb_define_method(ruby_attrib_class, "attachment", ruby_attrib_attachment_get, 0);
    rb_define_method(ruby_attrib_class, "attachment=", ruby_attrib_attachment_set, 1);
    rb_define_method(ruby_attrib_class, "attach!", ruby_attrib_attachment_set, 1);
    rb_define_method(ruby_attrib_class, "detach!", ruby_attrib_detach, 0);
    rb_define_method(ruby_attrib_class, "inherited?", ruby_attrib_inherited_get, 0);

    ruby_component_class = rb_define_class_under(ruby_geda_module, "Component", ruby_object_class);
    //rb_define_method(ruby_component_class, "initialize", ruby_component_initialize, 5);
    rb_define_method(ruby_component_class, "set", ruby_component_set, 4);
    rb_define_method(ruby_component_class, "basename", ruby_component_basename_get, 0);
    rb_define_method(ruby_component_class, "position", ruby_component_position_get, 0);
    rb_define_method(ruby_component_class, "position", ruby_component_position_set, 1);
    rb_define_method(ruby_component_class, "angle", ruby_component_angle_get, 0);
    rb_define_method(ruby_component_class, "angle", ruby_component_angle_set, 1);
    rb_define_method(ruby_component_class, "mirror", ruby_component_mirror_get, 0);
    rb_define_method(ruby_component_class, "mirror", ruby_component_mirror_set, 1);
    rb_define_method(ruby_component_class, "locked", ruby_component_locked_get, 0);
    rb_define_method(ruby_component_class, "locked", ruby_component_locked_set, 1);
    rb_define_method(ruby_component_class, "contents", ruby_component_contents_get, 0);
    rb_define_method(ruby_component_class, "append!", ruby_component_append, -1);
    rb_define_method(ruby_component_class, "remove!", ruby_component_remove, -1);
    //rb_define_method(ruby_component_class, "inheritable_attribs", ruby_component_inheritable_attribs_get, 0);
    //rb_define_method(ruby_component_class, "promotable_attribs", ruby_component_inheritable_attribs_get, 0);
    //rb_define_method(ruby_component_class, "promote_attribs!", ruby_component_promote_attribs, -1);
}

