#include "./repl.hpp"
#include <regex>
#include <iostream>
using std::cout;
using std::cerr;
using std::endl;

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
bool Repl::handleInput(const char* str, bool clearPreviousInput) {

    if (clearPreviousInput) {
        input_buffer.clear();
    }
    // completing previous input that could not be read
    input_buffer += str;

//     cerr << "input str " << str << "buffer " << input_buffer << endl;

    std::string wrapped;
    // clojure style namespace.
    if (s7_boolean(sc, s7_eval_c_string(sc, "(and (defined? '*ns*) (let? *ns*))"))) {
        // the following commented block was causing problems
        // picture sending (ns foo) (do-that) and they all arrive together over network
        // .. then only the (ns foo) will run cause of how the reader works
//         if (std::regex_search(input_buffer, repl::NS_REGEXP)) {
        // if the input_buffer is "(ns ...)" then skip wrapping
        // that makes the eval-hook easy to recognize such eval'd forms
        // one just has to check (eq? 'ns (car (hook 'form)))
//             wrapped = input_buffer;
//         } else {
        // new line is important! (begin .... \n ) : might have comments in last line
        wrapped = "(with-let *ns* (begin " + input_buffer + "\n))";
//         }
    } else {
        // new line is important! (begin .... \n ) : might have comments in last line
        wrapped = "(begin " + input_buffer + "\n)";
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
        return false;
    } else {
//         cerr << "wrapped form " << wrapped << endl;;
        last_form = form;
        input_buffer.clear();
    }

    return true;
}

/**
 * Should be called after handleInput returns true
 */
std::string Repl::evalLastForm() {

    s7_pointer out = s7_open_output_string(sc);
    s7_pointer out_prev = s7_set_current_output_port(sc, out);

    s7_pointer res = s7_eval(sc, last_form, s7_nil(sc));
    char *res_str = s7_object_to_c_string(sc, res); // has to be freed

    const char *out_str = s7_get_output_string(sc, out);

    s7_pointer eval_hook = s7_eval_c_string(sc, "(and (defined? 'aod.c.repl) (aod.c.repl '*eval-hook*))");

    if (eval_hook != s7_f(sc)) {
        // s7_is_function not working with the hook (returns false)
        // if (s7_is_function(eval_hook)) {
        s7_call(sc, eval_hook,
                s7_list(sc,
                        3,
                        last_form,
                        res,
                        s7_make_string(sc, out_str)
                       )
               );
    } else {
        // std::cerr << "No eval hook??\n";
    }

    std::string str = res_str;
    s7_close_output_port(sc, out);
    s7_set_current_output_port(sc, out_prev);

    delete[] res_str;

    return str;
}

namespace repl {

// aod.c.repl bindings
// *eval-hook* etc
const std::regex NS_REGEXP("^\\(ns [a-zA-Z.-]+\\)");

void bind(s7_scheme* sc) {
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    // not sure about eval with evnironment or not
    s7_pointer eval_hook = s7_eval_c_string_with_environment(sc, "(make-hook 'form 'res 'out)", env);

    s7_define(sc, env, s7_make_symbol(sc, "*eval-hook*"),
              eval_hook);

    s7_define_variable(sc, "aod.c.repl", env);
}
} // repl

} // s7

} // aod

