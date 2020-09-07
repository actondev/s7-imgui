#pragma once

#include <functional>
#include <memory>

namespace aod {
namespace net {

typedef std::function<const std::string(const char*)> Callback;

// a dummy cross-platform tcp server
class TcpServer {
private:
    Callback cb;
    std::string init_msg;
    bool running = false;
    
    // hiding impl details (eg TCPsocket in case of SDL etc..)
    class Impl;
    std::unique_ptr<Impl> pimpl;
public:
    TcpServer();
    ~TcpServer();
    int listen(int port, Callback cb);
    int listen(int port, Callback cb, std::string init_msg);
};

} // net
} // aod

