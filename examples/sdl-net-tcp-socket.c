// from https://moddb.fandom.com/wiki/SDL:Tutorial:Using_SDL_net
#include <stdio.h>
#include <stdlib.h>
#include <string.h> 

#include "SDL_net.h"
#define PORT 1234
#define BUF_SIZE 16

int main(int argc, char **argv) {
	TCPsocket sd, csd; /* Socket descriptor, Client socket descriptor */
	IPaddress ip, *remoteIP;
	int quit;
	char buffer[512];

	if (SDLNet_Init() < 0) {
		fprintf(stderr, "SDLNet_Init: %s\n", SDLNet_GetError());
		exit(EXIT_FAILURE);
	}

	/* Resolving the host using NULL make network interface to listen */
	if (SDLNet_ResolveHost(&ip, NULL, 1234) < 0) {
		fprintf(stderr, "SDLNet_ResolveHost: %s\n", SDLNet_GetError());
		exit(EXIT_FAILURE);
	}

	/* Open a connection with the IP provided (listen on the host's port) */
	if (!(sd = SDLNet_TCP_Open(&ip))) {
		fprintf(stderr, "SDLNet_TCP_Open: %s\n", SDLNet_GetError());
		exit(EXIT_FAILURE);
	}

	printf("listening on port %d\n", PORT);

	/* Wait for a connection, send data and term */
	quit = 0;
	while (!quit) {
		/* This check the sd if there is a pending connection. * If there is one, accept that, and open a new socket for communicating */
		if ((csd = SDLNet_TCP_Accept(sd))) {
			/* Now we can communicate with the client using csd socket * sd will remain opened waiting other connections */

			/* Get the remote address */
			if ((remoteIP = SDLNet_TCP_GetPeerAddress(csd))) {
				/* Print the address, converting in the host format */
				printf("Host connected: %x %d\n",
						SDLNet_Read32(&remoteIP->host),
						SDLNet_Read16(&remoteIP->port));
			} else {
				fprintf(stderr, "SDLNet_TCP_GetPeerAddress: %s\n",
						SDLNet_GetError());
			}

			int countRecv = 0;
			for (;;) {
				countRecv = SDLNet_TCP_Recv(csd, buffer, BUF_SIZE);
				if (countRecv <= 0) {
				  // disconnected or some other problem
				  break;
				}
				// buffer[countRecv-1] should be ascii 10 : LF (line feed)
				/* printf("last char is %d\n", buffer[countRecv-1]); */
				buffer[countRecv] = 0; // terminating what we read
				printf("Client say: %s\n", buffer);
			}
			/* Close the client socket */
			SDLNet_TCP_Close(csd);
		}
	}

	SDLNet_TCP_Close(sd);
	SDLNet_Quit();

	return EXIT_SUCCESS;
}
