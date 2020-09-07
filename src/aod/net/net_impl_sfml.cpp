#include "./net.hpp"
#include <string>
#include <sstream>
#include <iostream>
#include <stdio.h>
#include <thread>

// impl
#include <SFML/Network.hpp>


namespace aod {
namespace net {
// is const auto working..?
// const auto BUFFER_SIZE = 512;
#define BUFFER_SIZE 512

class TcpServer::Impl {
public:
    TcpServer* inst;
    sf::TcpListener listener;
    Impl(TcpServer* inst) {
        this->inst = inst;
    }
    static int listenLoop(void* data) {
        TcpServer* that = (TcpServer*)data;
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
    fprintf(stderr, "TcpServer::listen (sfml impl)\n");
    this->cb = cb;
    this->init_msg = init_msg;
    new std::thread(Impl::listenLoop, (void*)this);

}

} // net
} // aod

