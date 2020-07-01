#include "s7.h"
#include <sstream>
#include <iostream>

namespace aod {
namespace s7 {

namespace {
std::ostringstream _out_stream;
void _print_stderr(s7_scheme *sc, uint8_t c, s7_pointer port) {
	fprintf(stderr, "%c", c);
}
void _print_temp(s7_scheme *sc, uint8_t c, s7_pointer port) {
	_out_stream << c;
}
} // anonymous


void set_print_stderr(s7_scheme *sc) {
	s7_set_current_output_port(sc, s7_open_output_function(sc, _print_stderr));
}

void load_file(s7_scheme *sc, const char *file) {
	if (!s7_load(sc, file)) {
		fprintf(stderr, "can't load %s\n", file);
	}
}

/**
 * Wraps the passed sexp around a (write ..) call,
 * and returns the written output.
 */
std::string eval_write(s7_scheme *sc, const char *sexp) {
	std::ostringstream wrapped_sexp;
	// wrapping around begin as well to handle empty input (not enough arguments passed to write error)
	wrapped_sexp << "(write (begin " << sexp << "))";

	s7_pointer old_port = s7_set_current_output_port(sc,
			s7_open_output_function(sc, _print_temp));

	s7_pointer old_error_port = s7_set_current_error_port(sc,
				s7_open_output_function(sc, _print_temp));

	_out_stream.clear();
	_out_stream.str("");
	s7_eval_c_string(sc, wrapped_sexp.str().c_str());

	// reverting
	s7_set_current_output_port(sc, old_port);
	s7_set_current_error_port(sc, old_error_port);

	return _out_stream.str();
}

} // s7
} // aod
