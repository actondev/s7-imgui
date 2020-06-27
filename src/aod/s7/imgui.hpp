#pragma once

#include "imgui.hpp"
#include "s7.h"

namespace aod {
namespace s7 {
namespace imgui {

namespace {

static s7_pointer add1(s7_scheme *sc, s7_pointer args)
{
  /* all added functions have this form, args is a list,
   ,*    s7_car(args) is the first arg, etc
   ,*/

  /* the recommendation is to check every single argument yourself
     and return an error to the scheme caller of this functions
   ,*/
  if (!s7_is_integer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, "add1", 1, s7_car(args), "an integer"));

  // after you do all the checks, return the proper result
  return(s7_make_integer(sc, 1 + s7_integer(s7_car(args))));
}

} // demo

namespace { // anonymous namespace: the functions

s7_pointer begin(s7_scheme *sc, s7_pointer args) {
	s7_pointer title = s7_car(args);
	if (!s7_is_string(title))
		return (s7_wrong_type_arg_error(sc, "aod.s7/begin", 1, title,
				"First argument is title, should be a string"));

	const char *str = s7_string(title);
	ImGui::Begin(str);
	return (s7_nil(sc));
}

s7_pointer end(s7_scheme *sc, s7_pointer args) {
	ImGui::End();
	return (s7_nil(sc));
}

s7_pointer text(s7_scheme *sc, s7_pointer args) {
	s7_pointer text = s7_car(args);
	if (!s7_is_string(text))
		return (s7_wrong_type_arg_error(sc, "aod.s7/text", 1, text,
				"test should get a string argument"));

	ImGui::Text("%s", s7_string(text));
	return (s7_nil(sc));
}

} // ! anonymous namespace: the functions

void bind(s7_scheme *sc) {
	s7_define_function(sc, "aod.imgui/begin", begin, // ..
			1, // req args
			0, // optional args
			false, // rest args
			"Begin a window");

	s7_define_function(sc, "aod.imgui/end", end, // ..
			0, // req args
			0, // optional args
			false, // rest args
			"End a window");

	s7_define_function(sc, "aod.imgui/text", text, // ..
			1, // req args
			0, // optional args
			false, // rest args
			"Draw a text label");

	s7_define_function(sc, "add1", add1, 1, 0, false, "(add1 int) adds 1 to int");
}


} // imgui
} // s7
} // aod
