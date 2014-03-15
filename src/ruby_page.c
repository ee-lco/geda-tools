#include "libgedaruby.h"

VALUE ruby_page_class;

static VALUE ruby_page_alloc(VALUE class);
static void ruby_page_mark(void *obj);
static void ruby_page_free(void *obj);
static VALUE ruby_page_initialize(int argc, VALUE argv[], VALUE self);
static VALUE ruby_page_get_filename(VALUE self);
static VALUE ruby_page_set_filename(VALUE self, VALUE filename);
static VALUE ruby_page_read(VALUE self, VALUE string);
static VALUE ruby_page_write(VALUE self);


VALUE ruby_page_from_c(PAGE *page)
{
fprintf(stderr, "%s(%p)\n", __func__, page);
    return Data_Wrap_Struct(ruby_page_class, ruby_page_mark, ruby_page_free, page);
}

static VALUE ruby_page_alloc(VALUE class)
{
fprintf(stderr, "%s()\n", __func__);
    TOPLEVEL *toplevel;
    PAGE *page;
    
    toplevel = ruby_get_c_toplevel();
    page = s_page_new(ruby_get_c_toplevel(), "untitled");

    return ruby_page_from_c(page);
}

static void ruby_page_mark(void *obj)
{
fprintf(stderr, "%s(%p)\n", __func__, obj);
    ///@todo
}

static void ruby_page_free(void *obj)
{
fprintf(stderr, "%s(%p)\n", __func__, obj);
    ///@todo
#if 0
    PAGE *page;
    
    Data_Get_Struct(obj, PAGE, page);
    s_page_delete(ruby_get_c_toplevel(), page);
#endif
}

static VALUE ruby_page_initialize(int argc, VALUE argv[], VALUE self)
{
fprintf(stderr, "%s()\n", __func__);
    VALUE filename;
    VALUE string;
    PAGE *page;

    Data_Get_Struct(self, PAGE, page);

    rb_scan_args(argc, argv, "11", &filename, &string);

    ruby_page_set_filename(self, filename);

    if (!NIL_P(string)) {
        ruby_page_read(self, string);
    }

    return self;
}

static VALUE ruby_page_get_filename(VALUE self)
{
    PAGE *page;
    
    Data_Get_Struct(self, PAGE, page);

    return rb_str_new2(page->page_filename);
}

static VALUE ruby_page_set_filename(VALUE self, VALUE filename)
{
    PAGE *page;
    char *page_filename;
    
    Check_Type(filename, T_STRING);

    Data_Get_Struct(self, PAGE, page);

    if (page->page_filename != NULL) {
        g_free(page->page_filename);
    }

    page_filename = g_strdup(StringValueCStr(filename));
    ///@todo absolute path handling should be in libgeda
    if (g_path_is_absolute (page_filename)) {
        page->page_filename = g_strdup(page_filename);
    } else {
        gchar *pwd = g_get_current_dir();
        page->page_filename = g_build_filename(pwd, page_filename, NULL);
        g_free(pwd);
    }

    return self;
}

static VALUE ruby_page_read(VALUE self, VALUE string)
{
    TOPLEVEL *toplevel;
    PAGE *page;
    GError *err;
    char *str;
    size_t len;
    GList *objects;

    Check_Type(string, T_STRING);

    toplevel = ruby_get_c_toplevel();
    Data_Get_Struct(self, PAGE, page);
    err = NULL;
    str = g_strdup(StringValueCStr(string));
    len = strlen(str);

    objects = o_read_buffer(ruby_get_c_toplevel(), NULL, str, len, page->page_filename, &err);
    g_free(str);

    if (err) {
fprintf(stderr, "%s()\n", __func__);
fprintf(stderr, "ERROR: %s\n", err->message);
        ///@todo
    }

    s_page_append_list(ruby_get_c_toplevel(), page, objects);

    return self;
}

static VALUE ruby_page_write(VALUE self)
{
    TOPLEVEL *toplevel;
    PAGE *page;
    gchar *string;

    toplevel = ruby_get_c_toplevel();
    Data_Get_Struct(self, PAGE, page);

    string = o_save_buffer(toplevel, s_page_objects (page));

    return rb_str_new2(string);
}

void ruby_page_init(void)
{
    ruby_page_class = rb_define_class_under(ruby_geda_module, "Page", rb_cObject);
    rb_define_alloc_func(ruby_page_class, ruby_page_alloc);
    rb_define_method(ruby_page_class, "initialize", ruby_page_initialize, -1);
    //rb_define_method(ruby_page_class, "contents", ruby_page_contents, 1);
    //rb_define_method(ruby_page_class, "append", ruby_page_append, 1);
    //rb_define_method(ruby_page_class, "remove", ruby_page_remove, 1);
    rb_define_method(ruby_page_class, "filename", ruby_page_get_filename, 0);
    rb_define_method(ruby_page_class, "filename!=", ruby_page_set_filename, 1);
    //rb_define_method(ruby_page_class, "dirty?", ruby_page_get_dirty, 0);
    //rb_define_method(ruby_page_class, "dirty!", ruby_page_set_dirty, 0);
    rb_define_method(ruby_page_class, "read!", ruby_page_read, 1);
    rb_define_method(ruby_page_class, "write", ruby_page_write, 0);
    //rb_define_alias(ruby_page_class, "to_s", "write");
    //rb_define_alias(ruby_page_class, "[]", "contents");
}

