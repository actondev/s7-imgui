#include "s7.h"
#include <nlohmann/json.hpp>
namespace aod {
namespace s7 {
namespace json {

// namespace js = nlohmann;

int tag_json(s7_scheme* sc, s7_pointer env) {
    // fuck, not working
//     s7_pointer res = s7_eval_c_string_with_environment(sc, "type-json", s7_curlet(sc));
    
    // neither this (passing curlet from the parse function
//     s7_pointer res = s7_eval_c_string_with_environment(sc, "type-json", env);
    s7_pointer res = s7_eval_c_string(sc, "(aod.c.json 'type-json)");
    
    if (s7_is_integer(res)) {
        return s7_integer(res);
    }
    s7_error(sc,
             s7_make_symbol(sc, "error"),
             s7_cons(sc, s7_make_string(sc, "type-json not found"), s7_nil(sc)));
    return -1;
}

/**
 * old style
 void s7_c_type_set_free         (s7_scheme *sc, s7_int tag, void (*gc_free)(void *value));

 new style free/mark/equal and equivalent

 void s7_c_type_set_gc_free      (s7_scheme *sc, s7_int tag, s7_pointer (*gc_free)   (s7_scheme *sc, s7_pointer obj));

 */
s7_pointer free_json_new_style(s7_scheme* sc, s7_pointer obj) {
    nlohmann::basic_json<>* p_json = (nlohmann::basic_json<>*) s7_object_value(obj);
    printf("free json destructor %p\n", p_json);
    delete p_json;

    return s7_nil(sc);
}

s7_pointer ref_json(s7_scheme* sc, s7_pointer args) {
    printf("ref json\n");
    // (json* key-string)
    auto p_json = (nlohmann::basic_json<>*)s7_object_value(s7_car(args));

    printf("ref json pointer %p\n", p_json);

    // WIP just return a string
    // TODO check return value

    // TODO accept multiple args, nested at()?

    // eg (json* "foo" "bar")
    // performs a json["foo"]["bar"]

    args = s7_cdr(args);
    const char* key = s7_string(s7_car(args));

    std::string str = p_json->at(key).get<std::string>();

    return s7_make_string(sc, str.c_str());
}

s7_pointer parse(s7_scheme* sc, s7_pointer args) {
    const char* json_str = s7_string(s7_car(args));
    nlohmann::basic_json json = nlohmann::json::parse(json_str);

    nlohmann::basic_json<>* json_heap = new nlohmann::basic_json<>(json);

    printf("ref c object %p\n", json_heap);

    return s7_make_c_object(sc, tag_json(sc, s7_curlet(sc)), (void*)json_heap);
//     return s7_nil(sc);
}

void bind(s7_scheme* sc) {
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    // c object type
    s7_int type = s7_make_c_type(sc, "<json>");
    s7_define(sc, env, s7_make_symbol(sc, "type-json"),
              s7_make_integer(sc, type));
    s7_c_type_set_gc_free(sc, type, free_json_new_style);
    s7_c_type_set_ref(sc, type, ref_json);

    // functions
    s7_define(sc, env, s7_make_symbol(sc, "parse"),
              s7_make_function(sc, "parse", parse, 1, 0, false,
                               "(parse json-str) Returns a json c-object"));


    s7_define_variable(sc, "aod.c.json", env);
}
}
}
}
