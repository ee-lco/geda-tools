#include "libgedaruby.h"

VALUE ruby_page_class;

static VALUE ruby_page_alloc(VALUE class);
static void ruby_page_mark(void *obj);
static void ruby_page_free(void *obj);
static VALUE ruby_page_pages_index(VALUE class, VALUE key);
static VALUE ruby_page_pages_each(VALUE self);
static VALUE ruby_page_initialize(int argc, VALUE argv[], VALUE self);
static VALUE ruby_page_contents_each(VALUE self);
static VALUE ruby_page_append(int argc, VALUE argv[], VALUE self);
static VALUE ruby_page_remove(int argc, VALUE argv[] , VALUE self);
static VALUE ruby_page_get_filename(VALUE self);
static VALUE ruby_page_set_filename(VALUE self, VALUE filename);
static VALUE ruby_page_get_dirty(VALUE self);
static VALUE ruby_page_set_dirty(VALUE self, VALUE dirty);
static VALUE ruby_page_read(VALUE self, VALUE string);
static VALUE ruby_page_write(VALUE self);


VALUE ruby_page_from_c(PAGE *page)
{
    return Data_Wrap_Struct(ruby_page_class, ruby_page_mark, ruby_page_free, page);
}

PAGE *ruby_page_to_c(VALUE page)
{
    PAGE *pg;

    Data_Get_Struct(page, PAGE, pg);

    return pg;
}

static VALUE ruby_page_alloc(VALUE class)
{
    TOPLEVEL *toplevel;
    PAGE *page;

    toplevel = ruby_get_c_toplevel();
    page = s_page_new(ruby_get_c_toplevel(), "untitled");

    return ruby_page_from_c(page);
}

static void ruby_page_mark(void *obj)
{
    ///@todo
}

static void ruby_page_free(void *obj)
{
    ///@todo
#if 0
    PAGE *page;

    page = ruby_page_from_c(obj);
    s_page_delete(ruby_get_c_toplevel(), page);
#endif
}

static VALUE ruby_page_pages_index(VALUE class, VALUE key)
{
    TOPLEVEL *toplevel;
    unsigned index;
    char *filename;
    VALUE page;

    toplevel = ruby_get_c_toplevel();

    GList *page_list = geda_list_get_glist(toplevel->pages);
    if (TYPE(key) == T_FIXNUM) {
        index = NUM2INT(key);
        while (page_list != NULL && index >= 0) {
            if (index == 0) {
                return ruby_page_from_c(page_list->data);
            }
            page_list = g_list_next(page_list);
            index--;
        }
    } else if (TYPE(key) == T_STRING) {
        filename = StringValueCStr(key);
        while (page_list != NULL) {
            if (strcmp(filename, ((PAGE *)page_list->data)->page_filename) == 0) {
                return ruby_page_from_c(page_list->data);
            }
            page_list = g_list_next(page_list);
        }
    }
    return Qnil;
}

static VALUE ruby_page_pages_each(VALUE self)
{
    TOPLEVEL *toplevel;

    toplevel = ruby_get_c_toplevel();

    GList *page_list = geda_list_get_glist(toplevel->pages);
    while (page_list != NULL) {
        rb_yield(ruby_page_from_c(page_list->data));
        page_list = g_list_next(page_list);
    }

    return Qnil;
}


static VALUE ruby_page_initialize(int argc, VALUE argv[], VALUE self)
{
    PAGE *page;
    VALUE filename;
    VALUE string;

    page = ruby_page_to_c(self);

    rb_scan_args(argc, argv, "11", &filename, &string);

    ruby_page_set_filename(self, filename);

    if (!NIL_P(string)) {
        ruby_page_read(self, string);
    }

    return self;
}

static VALUE ruby_page_contents_each(VALUE self)
{
    PAGE *page;

    page = ruby_page_to_c(self);

    const GList *object_list = s_page_objects(page);
    while (object_list != NULL) {
        rb_yield(ruby_object_from_c(object_list->data));
        object_list = g_list_next(object_list);
    }

    return Qnil;
}

static VALUE ruby_page_append(int argc, VALUE argv[], VALUE self)
{
    TOPLEVEL *toplevel;
    PAGE *page;
    OBJECT *object;
    int i;

    toplevel = ruby_get_c_toplevel();
    page = ruby_page_to_c(self);

    for (i = 0; i < argc; i++) {
        object = ruby_object_to_c(argv[i]);

        ///@todo

        s_page_append(toplevel, page, object);
    }
    page->CHANGED = 1;

    return Qnil;
}

static VALUE ruby_page_remove(int argc, VALUE argv[] , VALUE self)
{
    TOPLEVEL *toplevel;
    PAGE *page;
    OBJECT *object;
    int i;

    toplevel = ruby_get_c_toplevel();
    page = ruby_page_to_c(self);

    for (i = 0; i < argc; i++) {
        object = ruby_object_to_c(argv[i]);

        ///@todo

        s_page_remove(toplevel, page, object);
    }
    page->CHANGED = 1;

    return Qnil;
}

static VALUE ruby_page_get_filename(VALUE self)
{
    PAGE *page;

    page = ruby_page_to_c(self);

    return rb_str_new2(page->page_filename);
}

static VALUE ruby_page_set_filename(VALUE self, VALUE filename)
{
    PAGE *page;
    char *page_filename;

    Check_Type(filename, T_STRING);

    page = ruby_page_to_c(self);

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

static VALUE ruby_page_get_dirty(VALUE self)
{
    PAGE *page;

    page = ruby_page_to_c(self);

    return page->CHANGED ? Qtrue : Qfalse;
}

static VALUE ruby_page_set_dirty(VALUE self, VALUE dirty)
{
    PAGE *page;

    page = ruby_page_to_c(self);

    page->CHANGED = RTEST(dirty) ? 1 : 0;

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
    page = ruby_page_to_c(self);
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
    page = ruby_page_to_c(self);

    string = o_save_buffer(toplevel, s_page_objects(page));

    return rb_str_new2(string);
}

void ruby_page_init(void)
{
    ruby_page_class = rb_define_class_under(ruby_geda_module, "Page", rb_cObject);
    rb_define_singleton_method(ruby_page_class, "[]", ruby_page_pages_index, 1);
    rb_define_singleton_method(ruby_page_class, "pages", ruby_page_pages_each, 0);
    rb_define_alloc_func(ruby_page_class, ruby_page_alloc);
    rb_define_method(ruby_page_class, "initialize", ruby_page_initialize, -1);
    rb_define_method(ruby_page_class, "contents", ruby_page_contents_each, 0);
    rb_define_method(ruby_page_class, "append!", ruby_page_append, -1);
    rb_define_method(ruby_page_class, "remove!", ruby_page_remove, -1);
    rb_define_method(ruby_page_class, "filename", ruby_page_get_filename, 0);
    rb_define_method(ruby_page_class, "filename!=", ruby_page_set_filename, 1);
    rb_define_method(ruby_page_class, "dirty?", ruby_page_get_dirty, 0);
    rb_define_method(ruby_page_class, "dirty!", ruby_page_set_dirty, 1);
    rb_define_method(ruby_page_class, "read!", ruby_page_read, 1);
    rb_define_method(ruby_page_class, "write", ruby_page_write, 0);
}

