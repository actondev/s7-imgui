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

        sf::TcpSocket socket;
        char buffer[BUFFER_SIZE];
        std::size_t received;

        while (that->running) {
            if (that->pimpl->listener.accept(socket) != sf::Socket::Done) {
                // error...
                fprintf(stderr, "listener.accept not done\n");
                return -1;
            }

            // writing the init/welcome message
            if (socket.send(that->init_msg.c_str(), that->init_msg.size()) != sf::Socket::Done) {
                fprintf(stderr, "socket.send error. disconnecting client?\n");
                break;
            }

            for (;;) {
                // loop: receiving from the same client
                if (socket.receive(buffer, BUFFER_SIZE, received) != sf::Socket::Done) {
                    fprintf(stderr, "socket.receive error. client disconnected?\n");
                    break;
                }

                buffer[received] = 0; // terminating what we read
                std::string response = that->cb(buffer); // calling the callback

                if (socket.send(response.c_str(), response.size()) != sf::Socket::Done) {
                    fprintf(stderr, "socket.send error. disconnecting client?\n");
                    break;
                }

                // sleeping before we receive anything else from our client
                // maybe not needed..? but it's useful for locks etc
                std::this_thread::sleep_for(std::chrono::milliseconds(10));
            }
            socket.disconnect();

            // sleeping before we accept any new clients
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
        printf("TcpServer event loop quit\n");
        that->running = false;
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
TcpServer::~TcpServer() {
    fprintf(stderr, "~TcpServer\n");
    pimpl->listener.close();
}

int TcpServer::listen(int port, aod::net::Callback cb) {
    return listen(port, cb, "");
}

int TcpServer::listen(int port, aod::net::Callback cb, std::string init_msg) {
    fprintf(stderr, "TcpServer::listen (sfml impl)\n");
    if (running) {
        fprintf(stderr, "TCP server already running, skipping\n");
        return -1;
    }
    this->cb = cb;
    this->init_msg = init_msg;
    std::string addr = "localhost";
    bool opened = false;
    for(int i=0; i< 10; i++) {
        if (pimpl->listener.listen(port, sf::IpAddress(addr)) != sf::Socket::Done) {
            fprintf(stderr, "listener.listen not done\n");
//             return -1;
        } else {
            opened = true;
            break;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
    if(!opened) {
        fprintf(stderr, "Could not open listening port\n");
        return -1;
    }

    running = true;
    new std::thread(Impl::listenLoop, (void*)this);
    return 0;
}

} // net
} // aod



