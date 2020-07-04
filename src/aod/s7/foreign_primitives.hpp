#include "s7.h"

namespace aod {
     namespace s7 {
	  namespace foreign {
	       void bind_primitives(s7_scheme *sc, s7_pointer env);
	       inline void bind_primitives(s7_scheme *sc){
		    s7_pointer env = s7_inlet(sc, s7_nil(sc));
		    s7_gc_protect(sc, env);
		    bind_primitives(sc, env);
	       }
	       int tag_bool(s7_scheme* sc);
	       int tag_int(s7_scheme* sc);
	       int tag_float(s7_scheme* sc);
	  }
     }
}
