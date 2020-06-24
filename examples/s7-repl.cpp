/*
 * s7-repl.c
 *
 *  Created on: Jun 23, 2020
 *      Author: actondev
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "SDL.h"
#include "aod/path.hpp"
#include "s7.h"
#include "aod/s7.hpp"

int main(int argc, char **argv) {
	SDL_Init(SDL_INIT_EVERYTHING);
	char *path = SDL_GetBasePath();
	printf("base path is %s\n", path);
	aod::path::set(path);

	s7_scheme *sc = s7_init();
	aod::s7::set_print_stderr(sc);

	char sbuf[256];
	sprintf(sbuf, "%s%s", path, "scheme/");
	printf("new load path %s\n", sbuf);

	// s7_add_to_load_path(sc, sbuf);
	s7_add_to_load_path(sc, "scheme");
	aod::path::print_cwd();
	s7_load(sc, "repl.scm");
	s7_eval_c_string(sc, "((*repl* 'run))");
  
	free(path);
}
