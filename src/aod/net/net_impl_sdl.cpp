#include "./net.hpp"
#include <string>
#include <sstream>
#include <iostream>
#include <stdio.h>

// impl
#include "SDL_net.h"


namespace aod {
namespace net {
// is const auto working..?
// const auto BUFFER_SIZE = 512;
#define BUFFER_SIZE 512

class TcpServer::Impl {
public:
    TcpServer* inst;
    IPaddress ip;
    TCPsocket sd; // Socket descriptor
    Impl(TcpServer* inst) {
        this->inst = inst;
    }
    static int listenLoop(void* data) {

        TcpServer* that = (TcpServer*)data;
        TCPsocket csd; /* Client socket descriptor */

        // TODO grow if I get something that doesn't return in LF ?
        // but for now that will do
        char buffer[BUFFER_SIZE];

        printf("started listening\n");
        while (that->running) {
            // Check for pending connection. If there is one, accept & and open a new socket
            if ((csd = SDLNet_TCP_Accept(that->pimpl->sd))) {
                SDLNet_TCP_Send(csd, that->init_msg.c_str(),
                                that->init_msg.length());
                // could I check SDLNet_Read32(&remoteIP->host) here to see if from allowed network?

                int countRcv = 0;
                for (;;) {
                    countRcv = SDLNet_TCP_Recv(csd, buffer, BUFFER_SIZE - 1);
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

TcpServer::TcpServer()
    : pimpl{new Impl(this)}
{}

/**
 * https://herbsutter.com/gotw/_100/
 * about pimpl pattern
 *
You still need to write the visible class’ destructor yourself and
define it out of line in the implementation file, even if normally
it’s the same as what the compiler would generate. This is because
although both unique_ptr and shared_ptr can be instantiated with an
incomplete type, unique_ptr’s destructor requires a complete type in
order to invoke delete (unlike shared_ptr which captures more
information when it’s constructed). By writing it yourself in the
implementation file, you force it to be defined in a place where impl
is already defined, and this successfully prevents the compiler from
trying to automatically generate the destructor on demand in the
caller’s code where impl is not defined.
 */
TcpServer::~TcpServer() {}

int TcpServer::listen(int port, aod::net::Callback cb) {
    return listen(port, cb, "");
}

int TcpServer::listen(int port, aod::net::Callback cb, std::string init_msg) {
    fprintf(stderr, "TcpServer::listen\n");
    if (running) {
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
    char* host = NULL; // to listen, this has to be null
    if ((res = SDLNet_ResolveHost(&pimpl->ip, host, 1234)) < 0) {
        fprintf(stderr, "SDLNet_ResolveHost: %s\n", SDLNet_GetError());
        return res;
    }
    /* Open a connection with the IP provided (listen on the host's port) */
    if (!(pimpl->sd = SDLNet_TCP_Open(&pimpl->ip))) {
        fprintf(stderr, "SDLNet_TCP_Open: %s\n", SDLNet_GetError());
        return -1;
    }

    running = true;
    SDL_CreateThread(pimpl->listenLoop, "aod::net::TcpServer", (void*)this);
    fprintf(stderr, "listening on port %d\n", port);
    return 0;
}

} // net
} // aod

