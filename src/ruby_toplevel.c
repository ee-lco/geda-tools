#include "libgedaruby.h"

VALUE ruby_toplevel_class;

static VALUE ruby_toplevel_alloc(VALUE class);
static void ruby_toplevel_mark(void *obj);
static void ruby_toplevel_free(void *obj);
static VALUE ruby_toplevel_initialize(VALUE self);


TOPLEVEL *ruby_get_c_toplevel(void)
{
    static TOPLEVEL *toplevel = NULL;

    if (!toplevel) {
        toplevel = s_toplevel_new();
        i_vars_libgeda_set(toplevel);
    }

    return toplevel;
}


VALUE roby_toplevel_from_c(TOPLEVEL *toplevel)
{
fprintf(stderr, "%s(%p)\n", __func__, toplevel);
    return Data_Wrap_Struct(ruby_toplevel_class, ruby_toplevel_mark, ruby_toplevel_free, toplevel);
}

static VALUE ruby_toplevel_alloc(VALUE class)
{
fprintf(stderr, "%s()\n", __func__);
    TOPLEVEL *toplevel;
    
    toplevel = ruby_get_c_toplevel();

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

void ruby_toplevel_init(void)
{
    VALUE ruby_singleton;

    rb_require("singleton");
    ruby_singleton = rb_const_get(rb_cObject, rb_intern("Singleton"));

    ruby_toplevel_class = rb_define_class_under(ruby_geda_module, "Toplevel", rb_cObject);
    rb_include_module(ruby_toplevel_class, ruby_singleton);
    rb_funcall(ruby_singleton, rb_intern("included"), 1, ruby_toplevel_class);
    rb_define_alloc_func(ruby_toplevel_class, ruby_toplevel_alloc);
    rb_define_method(ruby_toplevel_class, "initialize", ruby_toplevel_initialize, 0);
    rb_define_method(ruby_toplevel_class, "active_pages", ruby_toplevel_active_pages, 0);
}

