#pragma once

#include "s7.h"
#include <cstddef>

namespace aod {
     namespace s7 {
	  namespace foreign {
	       // https://stackoverflow.com/a/17014793
	       // template <typename T, std::size_t S>
	       // inline
	       // std::size_t arr_size(const T (&v)[S])
	       // { 
	       // 	    return S; 
	       // }

	       void bind_primitives_arr(s7_scheme *sc, s7_pointer env);
	       inline void bind_primitives_arr(s7_scheme *sc){
		    s7_pointer env = s7_inlet(sc, s7_nil(sc));
		    s7_gc_protect(sc, env);
		    bind_primitives_arr(sc, env);
	       }
	       int tag_bool_arr(s7_scheme* sc);
	       int tag_int_arr(s7_scheme* sc);
	       int tag_float_arr(s7_scheme* sc);
	  }
     }
}
