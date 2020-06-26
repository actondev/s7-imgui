#include "aod/tcp_server.hpp"
#include "SDL.h"
#include <sstream>
#include <iostream>
#include <string.h>

int main(const int argc, const char **argv) {

	aod::TcpServer server;
	server.listen(1234, [](const char *data) -> std::string {
		printf("main: got data %s\n", data);

		std::ostringstream stream;
		stream << "Got " << strlen(data) << " chars" << std::endl;
		std::string str = stream.str();

		return str;
	});

	for (;;) {
		// printf("Main..\n");
		SDL_Delay(1000);
	}

}
