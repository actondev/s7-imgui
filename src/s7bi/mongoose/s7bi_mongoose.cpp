#include "s7bi_mongoose.hpp"
// I named this s7bi_mongoose.hpp instead of mongoose.hpp
// to be able to include "mongoose.hpp"
// but I gess this is possible with #include <mongoose.hpp>
#include "mongoose.hpp"
namespace s7bi {
namespace mongoose {



struct cb_data {
    mg_mgr* mgr;
    s7_scheme* sc;
    s7_pointer cb;
};

static void cb(struct mg_connection *c, int ev, void *ev_data, void *fn_data) {
    cb_data* data = (cb_data*)fn_data;
    s7_scheme* sc = data->sc;

    if (ev == MG_EV_HTTP_MSG) {
        s7_pointer res = s7_call(data->sc, data->cb, s7_nil(data->sc));
//         printf("s7 res %s", s7_string(res));
        struct mg_http_message *hm = (struct mg_http_message *) ev_data;
//         mg_http_serve_dir(c, hm, ".");
        mg_http_reply(c, 200, "", s7_string(res));
//         mg_http_reply(c, 200, "Content-Type: text/html\r\n", "hi there!\r\n");
//         mg_http_reply(c, 200, "", "");
    }
    (void) fn_data;
}



const char* help_listen_blocking = "(listen-blocking addr cb) TODO cb accepts no params";
s7_pointer listen_blocking(s7_scheme* sc, s7_pointer args) {

    struct mg_mgr mgr;
    struct mg_connection *c;

//     mg_log_set(s_debug_level);

    const char* s_listening_address = s7_string(s7_car(args));
    args = s7_cdr(args);

    s7_pointer scm_cb = s7_car(args);

    cb_data data = {
        .mgr = &mgr,
        .sc = sc,
        .cb = scm_cb
    };

    mg_mgr_init(&mgr);
    if ((c = mg_http_listen(&mgr, s_listening_address, cb, &data)) == NULL) {
        LOG(LL_ERROR, ("Cannot listen on %s. Use http://ADDR:PORT or :PORT",
                       s_listening_address));
        exit(EXIT_FAILURE);
    }
//     if (mg_casecmp(s_enable_hexdump, "yes") == 0) c->is_hexdumping = 1;

    // Start infinite event loop
    LOG(LL_INFO, ("Starting Mongoose web server v%s", MG_VERSION));
    for (;;) mg_mgr_poll(&mgr, 1000);
    mg_mgr_free(&mgr);
};

s7_pointer bind(s7_scheme* sc, s7_pointer args) {
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    s7_define(sc, env, s7_make_symbol(sc, "listen-blocking"),
              s7_make_function(sc, "listen-blocking", listen_blocking,
                               2, // req args
                               0, // optional args: thickness
                               false, // rest args
                               help_listen_blocking));


    // TODO s7bi.mongoose.c ? or s7bi.mongoose ?
    s7_define_variable(sc, "s7bi.mongoose", env);
    s7_eval_c_string(sc, "(format #t \"hiiiiiiiiiii\n\")");
    (void)args;

    return s7_nil(sc);
}

}
}
