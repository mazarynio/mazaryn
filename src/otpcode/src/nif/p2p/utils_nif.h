#ifndef UTILS_NIF_H
#define UTILS_NIF_H

#include <boost/asio.hpp>
#include <erl_nif.h>
#include <string>
#include <utility>
#include <iostream>

using boost::asio::ip::udp;

std::pair<std::string, int> getLocalIPAndPort();
std::pair<std::string, int> getPublicIPAndPort(const std::string& stun_server, unsigned short stun_port);

#endif // UTILS_NIF_H
