#include "./repl.hpp"

namespace aod {
namespace s7 {

void repl_print_stderr(s7_scheme *sc, uint8_t c, s7_pointer port) {
    fprintf(stderr, "%c", c);
}

Repl::Repl(s7_scheme *sc) {
    this->sc = sc;
    last_form = s7_nil(sc);

    // debug
    s7_set_current_output_port(sc,
                               s7_open_output_function(sc, repl_print_stderr));
}
/**
 * Returns true if the input string could be read (valid form).
 * Otherwise returns false.
 *
 * In case where the input doesn't constitute a valid form,
 * the input is stored and any following with the clearPreviousInput flag
 * set to false, build up the stored input until it constitues a valid form.
 *
 * If the result is true, you can then call the
 */
bool Repl::handleInput(std::string str, bool clearPreviousInput) {

    if (clearPreviousInput) {
        input_buffer.clear();
    }
    // completing previous input that could not be read
    input_buffer += str;

    std::string wrapped;
    // clojure style namespace.
    if (s7_boolean(sc, s7_eval_c_string(sc, "(and (defined? '*ns*) (let? *ns*))"))) {
        wrapped = "(with-let *ns* (begin " + input_buffer + "))";
    } else {
        wrapped = "(begin " + input_buffer + ")";
    }

    const char *c_str = wrapped.c_str();
    s7_pointer port = s7_open_input_string(sc, c_str);

    s7_pointer err = s7_open_output_string(sc);
    s7_pointer err_prev = s7_set_current_error_port(sc, err);

    // shall raise error if input isn't valid
    s7_pointer form = s7_read(sc, port);
    s7_close_input_port(sc, port);
    s7_set_current_error_port(sc, err_prev);

    const char *errmsg = s7_get_output_string(sc, err);
    s7_close_output_port(sc, err);
    if ((errmsg) && (*errmsg)) {
        std::string err_str = errmsg;
//			if (err_str.find(UNEXPECTED_CLOSE_PAREN) != std::string::npos) {
//				input_stream.clear();
//			}
        return false;
    } else {
        last_form = form;
        input_buffer.clear();
    }

    /*
     s7_pointer out = s7_open_output_string(sc);
     s7_pointer out_prev = s7_set_current_output_port(sc, out);
     s7_pointer res = s7_eval(sc, form, s7_nil(sc));
     if (!s7_is_valid(sc, res)) {
     fprintf(stderr, "res invalid valid\n");
     }
     //		s7_eval_c_string(sc, c_str);

     //		delete c_str;
     const char *out_str = s7_get_output_string(sc, out);
     //		char* out_str = s7_object_to_c_string(sc, out);
     const char *res_str = s7_object_to_c_string(sc, res);
     //		fprintf(stderr, "in handle input\n");
     printf("out is %s\n", out_str);
     printf("res is %s\n", res_str);
     //		delete out_str;
     */
    return true;
}

/**
 * Should be called after handleInput returns true
 */
std::string Repl::evalLastForm() {

    s7_pointer out = s7_open_output_string(sc);
    s7_pointer out_prev = s7_set_current_output_port(sc, out);
    s7_pointer res = s7_eval(sc, last_form, s7_nil(sc));

    // should I return this as well somehow?
//		const char *out_str = s7_get_output_string(sc, out);

//		char* out_str = s7_object_to_c_string(sc, out);
    char *res_str = s7_object_to_c_string(sc, res);
//		fprintf(stderr, "in handle input\n");
//		printf("out is %s\n", out_str);
//		printf("res is %s\n", res_str);
//		delete out_str;

    std::string str = res_str;
    s7_close_output_port(sc, out);
    s7_set_current_output_port(sc, out_prev);
    delete[] res_str;

    return str;
}

} // s7

} // aod

