#include "imgui.h"
#include "s7.h"
#include "aod/s7/c_primitives.hpp"

namespace aod {
namespace s7 {
namespace imgui {

namespace { // anonymous namespace: the functions

s7_pointer begin(s7_scheme *sc, s7_pointer args) {
	s7_pointer title = s7_car(args);
	if (!s7_is_string(title))
		return (s7_wrong_type_arg_error(sc, "imgui/begin", 1, title,
				"First argument is title, should be a string"));

	const char *str = s7_string(title);
	s7_pointer bool_open = s7_cadr(args);
	if (s7_is_c_object(bool_open)) {
		bool *p_open = (bool*) s7_c_object_value(bool_open);
		ImGui::Begin(str, p_open);
	} else {
		ImGui::Begin(str);
	}

	return (s7_nil(sc));
}

s7_pointer checkbox(s7_scheme *sc, s7_pointer args) {
	s7_pointer text = s7_car(args);
	if (!s7_is_string(text)) {
		return (s7_wrong_type_arg_error(sc, "imgui/checkbox", 1, text,
				"First argument is title, should be a string"));
	}

	s7_pointer bool_open = s7_cadr(args);
	if (!s7_is_c_object(bool_open)) {
		return (s7_wrong_type_arg_error(sc, "imgui/checkbox", 2, bool_open,
				"Second argument should be a c-object pointing to a c bool*"));
	}

	bool *p_check = (bool*) s7_c_object_value(bool_open);
	ImGui::Checkbox(s7_string(text), p_check);
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

s7_pointer color_edit_3(s7_scheme* sc, s7_pointer args){
	s7_pointer text = s7_car(args);
	if (!s7_is_string(text)) {
		return (s7_wrong_type_arg_error(sc, "imgui/color-edit-3", 1, text,
				"Expecting a string (title)"));
	}


	s7_pointer obj = s7_cadr(args);
	if (!s7_is_c_object(obj)) {
		return (s7_wrong_type_arg_error(sc, "imgui/color-edit-3", 2, obj,
				"Expecting a c-object pointing to a c float* array"));
	}

	float_arr* arr = (float_arr*) s7_c_object_value(obj);
	ImGui::ColorEdit3(s7_string(text), arr->elements);

}

} // ! anonymous namespace: the functions

void bind(s7_scheme *sc) {
	s7_pointer env = s7_inlet(sc, s7_nil(sc));
	s7_gc_protect(sc, env);

// TODO env
	s7_define_function(sc, "imgui/begin", begin, // ..
			1, // req args
			1, // optional args (the open boolean pointer)
			false, // rest args
			"Begin a window");

	s7_define_function(sc, "imgui/end", end, // ..
			0, // req args
			0, // optional args
			false, // rest args
			"End a window");

	s7_define_function(sc, "imgui/text", text, // ..
			1, // req args
			0, // optional args
			false, // rest args
			"Draw a text label");

	s7_define_function(sc, "imgui/button", button, // ..
			1, // req args
			   // TODO apparently there are optional args, about the size?
			0, // optional args
			false, // rest args
			"Draw a button. Returns a boolean, true if clicked");

	s7_define_function(sc, "imgui/checkbox", checkbox, // ..
			2, // req args
			0, // optional args
			false, // rest args
			"Checkbox");

	s7_define_function(sc, "imgui/color-edit-3", color_edit_3, // ..
			2, // req args
			0, // optional args
			false, // rest args
			"ColorEdit3");

}

} // imgui
} // s7
} // aod
