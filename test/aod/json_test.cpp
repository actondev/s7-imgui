#include "gtest/gtest.h"
#include <nlohmann/json.hpp>
#include "aod/s7/json.hpp"

using std::cout;
using std::endl;
TEST(json, basic) {
    const char* json_str = R"(
        {"previews":{"preview-lq-ogg":"https://freesound.org/data/previews/25/25667_48671-lq.ogg","preview-lq-mp3":"https://freesound.org/data/previews/25/25667_48671-lq.mp3","preview-hq-ogg":"https://freesound.org/data/previews/25/25667_48671-hq.ogg","preview-hq-mp3":"https://freesound.org/data/previews/25/25667_48671-hq.mp3"}}
        )";

    nlohmann::basic_json json = nlohmann::json::parse(json_str);
    nlohmann::basic_json value = json["previews"]["preview-lq-ogg"];
    ASSERT_TRUE(value.is_string());
    std::string value_str = value.get<std::string>();
    ASSERT_EQ(value_str, "https://freesound.org/data/previews/25/25667_48671-lq.ogg");
}

TEST(json, s7_simple_parse) {
    s7_scheme* sc = s7_init();
    aod::s7::json::bind(sc);

    s7_eval_c_string(sc, "(define parse (aod.c.json 'parse))");
    const char* json_str = R"(
    {"name":"Ram", "email":"Ram@gmail.com"}
    )";

    s7_define_variable(sc, "json-str", s7_make_string(sc, json_str));
    s7_eval_c_string(sc, "(define parsed (parse json-str))");

    const char* name = s7_string(s7_eval_c_string(sc, "(parsed \"name\")"));
    ASSERT_STREQ("Ram", name);

    s7_free(sc);
}

// valgrind --leak-check=full --show-reachable=yes --suppressions=test/s7.supp --suppressions=test/valgrind_gen.supp  ./build/test/gtest-all --gtest_filter="*valgrind*"
TEST(json, valgrind_s7_nested_values) {
    s7_scheme* sc = s7_init();
    aod::s7::json::bind(sc);


    const char* json_str = R"(
{
  "employees": [
    {
      "name": "Shyam",
      "email": "shyamjaiswal@gmail.com"
    },
    {
      "name": "Bob",
      "email": "bob32@gmail.com"
    },
    {
      "name": "Jai",
      "email": "jai87@gmail.com"
    }
  ],
  "numbers": {
    "int": 1,
    "double": 2.3,
    "negative double": -9.4
  }
}
)";
    s7_eval_c_string(sc, "(define parse (aod.c.json 'parse))");

    s7_define_variable(sc, "json-str", s7_make_string(sc, json_str));
    s7_eval_c_string(sc, "(define parsed (parse json-str))");
    
    
    const char* name1 = s7_string(s7_eval_c_string(sc, "(parsed \"employees\" 1 \"name\")"));
    ASSERT_STREQ("Bob", name1);
    
    int num_int = s7_integer(s7_eval_c_string(sc, "(parsed \"numbers\" \"int\")"));
    ASSERT_EQ(1, num_int);
    double num_double = s7_real(s7_eval_c_string(sc, "(parsed \"numbers\" \"double\")"));
    ASSERT_EQ(2.3, num_double);
    double num_neg_double = s7_real(s7_eval_c_string(sc, "(parsed \"numbers\" \"negative double\")"));
    ASSERT_EQ(-9.4, num_neg_double);
    
    
    // testing intermediate object
    // good thing to also test with valgrind
    s7_eval_c_string(sc, "(define employee#2 (parsed \"employees\" 2))");
    const char* mail2 = s7_string(s7_eval_c_string(sc, "(employee#2 \"email\")"));
    ASSERT_STREQ("jai87@gmail.com", mail2);
    
    s7_free(sc);
}
