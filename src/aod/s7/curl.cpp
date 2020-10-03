#include "s7.h"
#include <curl/curl.h>
#include <string>
#include <sstream>

namespace aod {
namespace s7 {
namespace curl {

// https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md
template<typename A>
struct final_action {   // slightly simplified
    A act;
    final_action(A a) : act{a} {}
    ~final_action() {
        act();
    }
};

template<typename A>
final_action<A> finally(A act) { // deduce action type
    return final_action<A> {act};
}

/**
 * The following were helpful:
 * - https://techoverflow.net/2013/03/15/simple-c-http-download-using-libcurl-easy-api/
 * - https://gist.github.com/alghanmi/c5d7b761b2c9ab199157
 */

size_t write_data_string(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

static size_t write_data_file(void *ptr, size_t size, size_t nmemb, void *stream) {
    size_t written = fwrite(ptr, size, nmemb, (FILE *)stream);
    return written;
}

const char* curl_default_opts = R"__(
(define *default-curl-opts*
 (inlet
   :ssl-verify-peer 1
   ;; 'no-signal 1
   :follow-location 1
   ))
;;)__";
const char* help_curl_star = "(curl url (out #f) (opts *default-curl-opts*))";
const char* args_curl_star = "url (out #f) (opts (inlet))";
s7_pointer curl_star(s7_scheme* sc, s7_pointer args) {

    const char* url = s7_string(s7_car(args));

    // out-file
    args = s7_cdr(args);
    s7_pointer pfile = s7_car(args);

    // curl-opts
    args = s7_cdr(args);
    s7_pointer passed_opts = s7_car(args);
    if (s7_is_list(sc, passed_opts)) {
        s7_pointer inlet = s7_name_to_value(sc, "inlet");
        passed_opts = s7_apply_function(sc,
                                        inlet,
                                        passed_opts);
    }

    s7_pointer default_opts = s7_eval_c_string(sc, "(aod.c.curl '*default-curl-opts*)");
    s7_pointer undefined = s7_undefined(sc);
    auto get_opt = [ = ](const char* opt_name)->int{
        s7_pointer symbol = s7_make_symbol(sc, opt_name);
        s7_pointer p = s7_let_ref(sc, passed_opts, symbol);
        if (p == undefined) {
            p = s7_let_ref(sc, default_opts, symbol);
        }
        return s7_integer(p);
    };

    CURL* curl = curl_easy_init();
    auto __curl = finally([curl] { curl_easy_cleanup(curl); });

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_NOSIGNAL, 1); //Prevent "longjmp causes uninitialized stack frame" bug
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, get_opt("follow-location"));
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, get_opt("ssl-verify-peer"));

    if (s7_is_string(pfile)) {
        FILE *outfile = fopen(s7_string(pfile), "wb");
        if (outfile) {
            // NOTE: even without the writefunction set, it WAS writting to outfile!
            // but it was slower I think
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data_file);
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, outfile);

            /* get it! */
            CURLcode res = curl_easy_perform(curl);
            if (res != CURLE_OK) {
                // todo s7 error
                fprintf(stderr, "curl_easy_perform() failed: %s\n",
                        curl_easy_strerror(res));
            }
            /* close the header file */
            fclose(outfile);
            return s7_make_boolean(sc, res == CURLE_OK);
        } else {
            fprintf(stderr, "Could not open file %s\n", s7_string(pfile));
            return s7_f(sc);
        }
    } else {
        // writing to a string and returning it
        std::string out;
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data_string);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &out);
        CURLcode res = curl_easy_perform(curl);
        /* Check for errors */
        if (res != CURLE_OK) {
            // todo s7 error
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
            return s7_f(sc);
        }
        return s7_make_string(sc, out.c_str());
    }
}

const char* help_easy_escape = "(easy-escape string)";
s7_pointer _curl_easy_escape(s7_scheme* sc, s7_pointer args) {
    CURL* curl = curl_easy_init();
    auto __curl = finally([curl] { curl_easy_cleanup(curl); });

    const char* str = s7_string(s7_car(args));
    char* escaped = curl_easy_escape(curl, str, 0);
    s7_pointer s_str = s7_make_string(sc, escaped);
    free(escaped);

    return s_str;
}

void bind(s7_scheme* sc) {
    // NOTE have to add something for cleanup
    curl_global_init(CURL_GLOBAL_ALL);

    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    s7_eval_c_string_with_environment(sc,
                                      curl_default_opts, env);

    s7_define(sc, env, s7_make_symbol(sc, "curl"),
              s7_make_function_star(sc, "curl", curl_star,
                                    args_curl_star, help_curl_star));

    s7_define(sc, env, s7_make_symbol(sc, "easy-escape"),
              s7_make_function(sc, "easy-escape", _curl_easy_escape,
                               1, 0, false, help_easy_escape));

    s7_define_variable(sc, "aod.c.curl", env);
}

}
}
}
