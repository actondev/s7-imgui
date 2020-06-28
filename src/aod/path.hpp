/*
 * aod_path.h
 *
 *  Created on: Jun 23, 2020
 *      Author: actondev
 */

#pragma once

#include <stdio.h>
#include <string>
//#include <stdlib.h>
//#include <string.h>

#ifdef __linux__
#include <unistd.h>
#endif

namespace aod {

namespace path {
void set(std::string path) {
#ifdef __linux__
	fprintf(stderr, "setting path to %s\n", path.c_str());
	chdir(path.c_str());
#else
	fprintf(stderr, "aod::path::set not implemented in this platform\n");
#endif
}

void print_cwd() {
#ifdef __linux__
	char cwd[512];
	if (getcwd(cwd, sizeof(cwd)) != NULL) {
		printf("Current working dir: %s\n", cwd);
	} else {
		perror("getcwd() error");
	}
#else
	   printf(stderr, "print_cwd not implemented\n");
#endif
}

} // path

} // aod

