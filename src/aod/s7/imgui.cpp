#include "imgui.h"
#include "s7.h"

namespace aod {
namespace s7 {
namespace imgui {

namespace { // anonymous namespace: the functions

s7_pointer begin(s7_scheme *sc, s7_pointer args) {
	s7_pointer title = s7_car(args);
	if (!s7_is_string(title))
		return (s7_wrong_type_arg_error(sc, "aod.s7/begin", 1, title,
				"First argument is title, should be a string"));

	const char *str = s7_string(title);
	s7_pointer bool_open = s7_cadr(args);
	if(s7_is_c_object(bool_open)){
		bool* p_open = (bool*)s7_c_object_value(bool_open);
		ImGui::Begin(str, p_open);
	} else {
		ImGui::Begin(str);
	}

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
				"text should get a string argument"));

	ImGui::Text("%s", s7_string(text));
	return (s7_nil(sc));
}

s7_pointer button(s7_scheme *sc, s7_pointer args) {
	s7_pointer text = s7_car(args);
	if (!s7_is_string(text))
		return (s7_wrong_type_arg_error(sc, "aod.s7/button", 1, text,
				"button should get a string argument"));

	bool clicked = ImGui::Button(s7_string(text));
	return (s7_make_boolean(sc, clicked));
}

} // ! anonymous namespace: the functions

void bind(s7_scheme *sc) {
	// TODO env
	s7_define_function(sc, "aod.imgui/begin", begin, // ..
			1, // req args
			1, // optional args (the open boolean pointer)
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

	s7_define_function(sc, "aod.imgui/button", button, // ..
			1, // req args
			// TODO apparently there are optional args, about the size?
			0, // optional args
			false, // rest args
			"Draw a button. Returns a boolean, true if clicked");
}


} // imgui
} // s7
} // aod
