#include "s7.h"

namespace aod {
     namespace s7 {
	  typedef struct {
	  	size_t size;
	  	bool* elements;
	  } bool_arr;
	  
	  typedef struct {
	  	size_t size;
	  	int* elements;
	  } int_arr;
	  
	  typedef struct {
	  	size_t size;
	  	float* elements;
	  } float_arr;
	  
	  
	  void bind_primitives_arr(s7_scheme *sc);
     }
}
