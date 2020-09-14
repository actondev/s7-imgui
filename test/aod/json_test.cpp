#include "gtest/gtest.h"
#include <nlohmann/json.hpp>
#include "aod/s7/json.hpp"

using std::cout;
using std::endl;
TEST(json, basic) {
    std::string json_str = "{\"previews\":{\"preview-lq-ogg\":\"https://freesound.org/data/previews/25/25667_48671-lq.ogg\",\"preview-lq-mp3\":\"https://freesound.org/data/previews/25/25667_48671-lq.mp3\",\"preview-hq-ogg\":\"https://freesound.org/data/previews/25/25667_48671-hq.ogg\",\"preview-hq-mp3\":\"https://freesound.org/data/previews/25/25667_48671-hq.mp3\"}}";

    nlohmann::basic_json json = nlohmann::json::parse(json_str);
    nlohmann::basic_json value = json["previews"]["preview-lq-ogg"];
    ASSERT_TRUE(value.is_string());
    std::string value_str = value.get<std::string>();
    ASSERT_EQ(value_str, "https://freesound.org/data/previews/25/25667_48671-lq.ogg");
}

TEST(json, valgrind_s7){
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
