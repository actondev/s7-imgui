#pragma once

#include <functional>
#include "SDL_net.h"

namespace aod {
namespace net {

typedef std::function<const std::string(const char*)> Callback;

// a dummy cross-platform tcp server with SDL_net
// one client only
class TcpServer {
private:
    bool running = false;
    IPaddress ip;
    TCPsocket sd; // Socket descriptor
    Callback cb;
    std::string init_msg;
    static int listenLoop(void* data);

public:
    TcpServer();
    int listen(int port, Callback cb);
    int listen(int port, Callback cb, std::string init_msg);
};

} // net
} // aod

