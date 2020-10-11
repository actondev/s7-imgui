#include "s7.h"
#include "stdio.h"

s7_pointer fun_star(s7_scheme* sc, s7_pointer) {
    return s7_nil(sc);
}

int main(int argc, char **argv) {
    s7_scheme* sc = s7_init();
    s7_define_function_star(sc,
                            "fun-star", fun_star,
                            "foo (opts (inlet 'foo \"bar\"))",
                            "Having the (inlet) as default variable crashes s7 when s7_free is called");

    s7_free(sc);
    printf("s7 freed\n");
}
