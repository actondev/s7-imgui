#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "nng/nng.h"
#include "nng/protocol/reqrep0/rep.h"
#include "nng/protocol/reqrep0/req.h"

void fatal(const char *func, int rv)
{
    fprintf(stderr, "%s: %s\n", func, nng_strerror(rv));
    exit(1);
}

int main(const int argc, const char **argv)
{
    printf("here\n");
    nng_socket sock;
    int        rv;

    if ((rv = nng_rep0_open(&sock)) != 0) {
	fatal("nng_rep0_open", rv);
    }
    if ((rv = nng_listen(sock, "tcp://127.0.0.1:1234", NULL, 0)) != 0) {
	fatal("nng_listen", rv);
    }
    fprintf(stderr, "Listening..\n");
    for (;;) {
	fprintf(stderr, "here..\n");
	char *   buf = NULL;
	size_t   sz;
	uint64_t val;
	if ((rv = nng_recv(sock, &buf, &sz, NNG_FLAG_ALLOC)) != 0) {
	    fprintf(stderr, "could not receive %d\n", rv);
	    fatal("nng_recv", rv);
	}
	fprintf(stderr, "Got msg:\n%s\n", buf);
	printf("Got msg:%s\n", buf);
	// Unrecognized command, so toss the buffer.
	rv = nng_send(sock, buf, sz, NNG_FLAG_ALLOC);
	/* nng_free(buf, sz); */
    }
}
