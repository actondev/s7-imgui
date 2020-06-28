#pragma once

#include <stdio.h>
#include <functional>
#include <string>
#include <sstream>
#include <iostream>
#include "SDL_net.h"
#include "SDL.h"

#define BUFFER_SIZE 512

namespace aod {
typedef std::function<const std::string(const char*)> Callback;

class TcpServer {
private:
	bool running;
	IPaddress ip;
	TCPsocket sd; // Socket descriptor
	Callback cb;
	std::string init_msg;
public:
	TcpServer() {
		running = false;
	}

	int listen(int port, Callback cb) {
		return listen(port, cb, "");
	}

	int listen(int port, Callback cb, std::string init_msg) {
		if(running){
			fprintf(stderr, "TCP server already running, skipping\n");
			return -1;
		}
		this->cb = cb;
		this->init_msg = init_msg;

		int res = 0;
		if ((res = SDLNet_Init()) < 0) {
			fprintf(stderr, "SDLNet_Init: %s\n", SDLNet_GetError());
			return res;
		}
		// jesus.. I cannot bind to "127.0.0.1" for listening.. this thing is open to the world
		char *host = NULL; // to listen, this has to be null
		if ((res = SDLNet_ResolveHost(&ip, host, 1234)) < 0) {
			fprintf(stderr, "SDLNet_ResolveHost: %s\n", SDLNet_GetError());
			return res;
		}
		/* Open a connection with the IP provided (listen on the host's port) */
		if (!(sd = SDLNet_TCP_Open(&ip))) {
			fprintf(stderr, "SDLNet_TCP_Open: %s\n", SDLNet_GetError());
			return -1;
		}

		running = true;
		SDL_CreateThread(listenLoop, "AOD: TcpServer", (void*) this);
		printf("listening on port %d\n", port);
		return 0;
	}

private:
	static int listenLoop(void *data) {
		TcpServer *that = (TcpServer*) data;
		TCPsocket csd; /* Client socket descriptor */

		// TODO grow if I get something that doesn't return in LF ?
		// but for now that will do
		char buffer[BUFFER_SIZE];

		printf("started listening\n");
		while (that->running) {
			// Check for pending connection. If there is one, accept & and open a new socket
			if ((csd = SDLNet_TCP_Accept(that->sd))) {
				SDLNet_TCP_Send(csd, that->init_msg.c_str(),
						that->init_msg.length());
				// could I check SDLNet_Read32(&remoteIP->host) here to see if from allowed network?

				int countRcv = 0;
				for (;;) {
					countRcv = SDLNet_TCP_Recv(csd, buffer, BUFFER_SIZE -1);
					if (countRcv <= 0) {
						// disconnected or some other problem
						break;
					}
					buffer[countRcv] = 0; // terminating what we read
					std::string response = that->cb(buffer); // calling the callback

					SDLNet_TCP_Send(csd, response.c_str(), response.length());
					SDL_Delay(10);
				}
				SDLNet_TCP_Close(csd);
			}
			SDL_Delay(10);
		}
		return 0;
	}
};
// class
}// aod namespace
