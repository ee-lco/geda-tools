#include <libgeda/libgeda.h>

#include <ruby.h>

static VALUE ruby_geda_module;

static VALUE ruby_toplevel_class;
static VALUE roby_toplevel_from_c(TOPLEVEL *toplevel);
static VALUE ruby_toplevel_alloc(VALUE class);
static void ruby_toplevel_mark(void *obj);
static void ruby_toplevel_free(void *obj);
static VALUE ruby_toplevel_initialize(VALUE self);

static VALUE ruby_page_class;
static VALUE ruby_page_from_c(PAGE *page);
static VALUE ruby_page_alloc(VALUE class);
static void ruby_page_mark(void *obj);
static void ruby_page_free(void *obj);
static VALUE ruby_page_initialize(int argc, VALUE argv[], VALUE self);
static VALUE ruby_page_get_filename(VALUE self);
static VALUE ruby_page_set_filename(VALUE self, VALUE filename);
static VALUE ruby_page_read(VALUE self, VALUE string);
static VALUE ruby_page_write(VALUE self);

static VALUE ruby_object_class;
static VALUE ruby_object_from_c(OBJECT *object);
static VALUE ruby_object_alloc(VALUE class);
static void ruby_object_mark(void *obj);
static void ruby_object_free(void *obj);


static TOPLEVEL *get_toplevel(void)
{
    static TOPLEVEL *toplevel = NULL;

    if (!toplevel) {
        toplevel = s_toplevel_new();
        i_vars_libgeda_set(toplevel);
    }

    return toplevel;
}


static VALUE roby_toplevel_from_c(TOPLEVEL *toplevel)
{
fprintf(stderr, "%s(%p)\n", __func__, toplevel);
    return Data_Wrap_Struct(ruby_toplevel_class, ruby_toplevel_mark, ruby_toplevel_free, toplevel);
}

static VALUE ruby_toplevel_alloc(VALUE class)
{
fprintf(stderr, "%s()\n", __func__);
    TOPLEVEL *toplevel;
    
    toplevel = get_toplevel();

    return roby_toplevel_from_c(toplevel);
}

static void ruby_toplevel_mark(void *obj)
{
fprintf(stderr, "%s(%p)\n", __func__, obj);
    ///@todo
}

static void ruby_toplevel_free(void *obj)
{
fprintf(stderr, "%s(%p)\n", __func__, obj);
    ///@todo
#if 0
    TOPLEVEL *toplevel;

    Data_Get_Struct(obj, TOPLEVEL, toplevel);
    s_toplevel_delete(toplevel);
#endif
}

static VALUE ruby_toplevel_initialize(VALUE self)
{
fprintf(stderr, "%s()\n", __func__);
    TOPLEVEL *toplevel;
    
    Data_Get_Struct(self, TOPLEVEL, toplevel);
    i_vars_libgeda_set(toplevel);

    return self;
}

static VALUE ruby_toplevel_active_pages(VALUE self)
{
    TOPLEVEL *toplevel;
    VALUE pages;
    VALUE page;
    
    Data_Get_Struct(self, TOPLEVEL, toplevel);
    pages = rb_ary_new();

    GList *page_list = geda_list_get_glist(toplevel->pages);
    while (page_list != NULL) {
        page = ruby_page_from_c(page_list->data);
        rb_ary_push(pages, page);
        page_list = g_list_next(page_list);
    }

    return pages;
}


static VALUE ruby_page_from_c(PAGE *page)
{
fprintf(stderr, "%s(%p)\n", __func__, page);
    return Data_Wrap_Struct(ruby_page_class, ruby_page_mark, ruby_page_free, page);
}

static VALUE ruby_page_alloc(VALUE class)
{
fprintf(stderr, "%s()\n", __func__);
    TOPLEVEL *toplevel;
    PAGE *page;
    
    toplevel = get_toplevel();
    page = s_page_new(get_toplevel(), "untitled");

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
    s_page_delete(get_toplevel(), page);
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

    toplevel = get_toplevel();
    Data_Get_Struct(self, PAGE, page);
    err = NULL;
    str = g_strdup(StringValueCStr(string));
    len = strlen(str);

    objects = o_read_buffer(get_toplevel(), NULL, str, len, page->page_filename, &err);
    g_free(str);

    if (err) {
fprintf(stderr, "%s()\n", __func__);
fprintf(stderr, "ERROR: %s\n", err->message);
        ///@todo
    }

    s_page_append_list(get_toplevel(), page, objects);

    return self;
}

static VALUE ruby_page_write(VALUE self)
{
    TOPLEVEL *toplevel;
    PAGE *page;
    gchar *string;

    toplevel = get_toplevel();
    Data_Get_Struct(self, PAGE, page);

    string = o_save_buffer(toplevel, s_page_objects (page));

    return rb_str_new2(string);
}

void Init_libgedaruby()
{
    VALUE ruby_singleton;

    scm_init_guile();
    libgeda_init();

    rb_require("singleton");
    ruby_singleton = rb_const_get(rb_cObject, rb_intern("Singleton"));

    ruby_geda_module = rb_define_module("GEDA");

    ruby_toplevel_class = rb_define_class_under(ruby_geda_module, "Toplevel", rb_cObject);
    rb_include_module(ruby_toplevel_class, ruby_singleton);
    rb_funcall(ruby_singleton, rb_intern("included"), 1, ruby_toplevel_class);
    rb_define_alloc_func(ruby_toplevel_class, ruby_toplevel_alloc);
    rb_define_method(ruby_toplevel_class, "initialize", ruby_toplevel_initialize, 0);
    rb_define_method(ruby_toplevel_class, "active_pages", ruby_toplevel_active_pages, 0);

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

    ruby_object_class = rb_define_class_under(ruby_geda_module, "Object", rb_cObject);
}

