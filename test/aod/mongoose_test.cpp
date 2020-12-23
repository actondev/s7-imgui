#include "gtest/gtest.h"
#include "mongoose.hpp"
#include "s7bi/mongoose/s7bi_mongoose.hpp"

static const char *s_debug_level = "2";
static const char *s_web_root_dir = ".";
static const char *s_listening_address = "http://localhost:8000";
static const char *s_enable_hexdump = "no";
static const char *s_rewrites = "";

static void cb(struct mg_connection *c, int ev, void *ev_data, void *fn_data) {
    if (ev == MG_EV_HTTP_MSG) {
        struct mg_http_message *hm = (struct mg_http_message *) ev_data;
//         mg_http_serve_dir(c, hm, s_web_root_dir);
        mg_http_reply(c, 200, "", "Hi there!");
    }
    (void) fn_data;
}

TEST(mongoose, hello_world) {
    GTEST_SKIP_("just testing the server");

    struct mg_mgr mgr;
    struct mg_connection *c;

    mg_log_set(s_debug_level);

    mg_mgr_init(&mgr);
    if ((c = mg_http_listen(&mgr, s_listening_address, cb, &mgr)) == NULL) {
        LOG(LL_ERROR, ("Cannot listen on %s. Use http://ADDR:PORT or :PORT",
                       s_listening_address));
        exit(EXIT_FAILURE);
    }
    if (mg_casecmp(s_enable_hexdump, "yes") == 0) c->is_hexdumping = 1;

    // Start infinite event loop
    LOG(LL_INFO, ("Starting Mongoose web server v%s", MG_VERSION));
    for (;;) mg_mgr_poll(&mgr, 1000);
    mg_mgr_free(&mgr);
}

TEST(mongoose, s7_hello_world) {
    s7_scheme* sc = s7_init();
    s7bi::mongoose::bind(sc, s7_nil(sc));
    
    s7_eval_c_string(sc, "(define listen-blocking (s7bi.mongoose 'listen-blocking))");
    s7_eval_c_string(sc, "(define (cb) (format #f \"hi from s7bi!\n\"))");
    s7_eval_c_string(sc, "(listen-blocking \"http://localhost:8000\" cb)");
    
}

