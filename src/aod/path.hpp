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

#ifdef __WIN32__
#include <direct.h>
#endif

namespace aod {

namespace path {
void set(std::string path) {
#ifdef __linux__
    fprintf(stderr, "setting path to %s\n", path.c_str());
    chdir(path.c_str());
#else
	_chdir(path.c_str());
#endif
}

char* get() {
#ifdef __linux__
    /**
     * The GNU library version of this function also permits you to specify a null pointer for the buffer argument.
     Then getcwd allocates a buffer automatically, as with malloc . If the size is greater than zero, then the
     buffer is that large; otherwise, the buffer is as large as necessary to hold the result.
     */
    return getcwd(NULL, 0);
#else
    fprintf(stderr, "cwd not implemented\n");
    exit(1);
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
	   fprintf(stderr, "print_cwd not implemented\n");
#endif
}

} // path

} // aod

