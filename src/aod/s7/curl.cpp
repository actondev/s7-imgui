#include "s7.h"
#include <curl/curl.h>
#include <string>
#include <sstream>

namespace aod {
namespace s7 {
namespace curl {

size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream) {
    std::string data((const char*) ptr, (size_t) size * nmemb);
    // Note: https://techoverflow.net/2013/03/15/simple-c-http-download-using-libcurl-easy-api/
    // the snippet there was streaming std::endl after the data... why??
    
    *((std::stringstream*) stream) << data;
    return size * nmemb;
}

const char* help_curl = "(curl url)";
s7_pointer curl(s7_scheme* sc, s7_pointer args) {
    const char* url = s7_string(s7_car(args));

    // https://techoverflow.net/2013/03/15/simple-c-http-download-using-libcurl-easy-api/
    CURL* curl = curl_easy_init();
    curl_easy_setopt(curl, CURLOPT_URL, url);
    /* example.com is redirected, so we tell libcurl to follow redirection */
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_NOSIGNAL, 1); //Prevent "longjmp causes uninitialized stack frame" bug

    // * SSL certificate problem: self signed certificate in certificate chain
    // TODO what should I do with this???
    // this happens ALWAYS
    // maybe I miss some configuration??
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);

    std::stringstream out;
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &out);

    CURLcode res = curl_easy_perform(curl);
    /* Check for errors */
    if (res != CURLE_OK) {
        // todo s7 error
        fprintf(stderr, "curl_easy_perform() failed: %s\n",
                curl_easy_strerror(res));
    }


    curl_easy_cleanup(curl);
    return s7_make_string(sc, out.str().c_str());
}

void bind(s7_scheme* sc) {
    // NOTE have to add something for cleanup
    curl_global_init(CURL_GLOBAL_ALL);

    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    s7_define(sc, env, s7_make_symbol(sc, "curl"),
              s7_make_function(sc, "curl", curl,
                               1, 0, false,
                               help_curl));


    s7_define_variable(sc, "aod.c.curl", env);
}

}
}
}
