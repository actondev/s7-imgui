#include "s7.h"
#include "aod/midi/midi.hpp"

namespace aod {
namespace s7 {
namespace midi {

s7_pointer is_note_on(s7_scheme* sc, s7_pointer args) {
    uint8_t status = s7_integer(s7_car(args));
    uint8_t data1 = s7_integer(s7_cadr(args));
    uint8_t data2 = s7_integer(s7_caddr(args));

    return s7_make_boolean(sc, aod::midi::is_note_on(status, data1, data2));
}

s7_pointer is_note_off(s7_scheme* sc, s7_pointer args) {
    uint8_t status = s7_integer(s7_car(args));
    uint8_t data1 = s7_integer(s7_cadr(args));
    uint8_t data2 = s7_integer(s7_caddr(args));

    return s7_make_boolean(sc, aod::midi::is_note_off(status, data1, data2));
}

s7_pointer note_number(s7_scheme* sc, s7_pointer args) {
    uint8_t status = s7_integer(s7_car(args));
    uint8_t data1 = s7_integer(s7_cadr(args));
    uint8_t data2 = s7_integer(s7_caddr(args));

    return s7_make_integer(sc, aod::midi::note_number(status, data1, data2));
}

void bind(s7_scheme* sc) {
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    s7_define(sc, env, s7_make_symbol(sc, "note-on?"),
              s7_make_function(sc, "note-on?", is_note_on, 3, 0, 0, "(note-on? status data1 data2)")
             );

    s7_define(sc, env, s7_make_symbol(sc, "note-off?"),
              s7_make_function(sc, "note-off?", is_note_off, 3, 0, 0, "(note-off? status data1 data2)")
             );

    s7_define(sc, env, s7_make_symbol(sc, "note-number"),
              s7_make_function(sc, "note-number", note_number, 3, 0, 0, "(note-number status data1 data2) Returns either the note or -1")
             );

    s7_define_constant(sc, "aod.c.midi", env);

}
} // midi
} //s7
}// aod
