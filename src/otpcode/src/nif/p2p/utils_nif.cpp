#include <boost/asio.hpp>
#include <erl_nif.h>
#include <string>
#include <utility>
#include <iostream>
#include <array>
#include <random>

using boost::asio::ip::udp;

const int STUN_BINDING_REQUEST = 0x0001;
const int STUN_BINDING_RESPONSE = 0x0101;
const int STUN_MAGIC_COOKIE = 0x2112A442;

struct StunHeader {
    uint16_t type;
    uint16_t length;
    uint32_t magic_cookie;
    uint8_t transaction_id[12];
};

#pragma pack(push, 1)
struct StunAttributeHeader {
    uint16_t type;
    uint16_t length;
};
#pragma pack(pop)

struct MappedAddress {
    uint8_t zero;
    uint8_t family;
    uint16_t port;
    boost::asio::ip::address_v4::bytes_type address;
};

uint16_t htons(uint16_t x) {
    return boost::asio::detail::socket_ops::host_to_network_short(x);
}

uint32_t htonl(uint32_t x) {
    return boost::asio::detail::socket_ops::host_to_network_long(x);
}

uint16_t ntohs(uint16_t x) {
    return boost::asio::detail::socket_ops::network_to_host_short(x);
}

uint32_t ntohl(uint32_t x) {
    return boost::asio::detail::socket_ops::network_to_host_long(x);
}

// Function to get local IP and port
std::pair<std::string, int> getLocalIPAndPort() {
    try {
        boost::asio::io_context io_context;

        // Create a UDP socket
        udp::socket socket(io_context, udp::endpoint(udp::v4(), 0));

        // Retrieve the local endpoint information
        udp::endpoint local_endpoint = socket.local_endpoint();

        // Extract IP address and port
        std::string ip_address = local_endpoint.address().to_string();
        int port = local_endpoint.port();

        return {ip_address, port};
    } catch (const std::exception& e) {
        std::cerr << "Exception while getting local IP and port: " << e.what() << std::endl;
        return {"", 0};
    }
}

// Function to create STUN Binding Request
std::array<uint8_t, 20> createStunBindingRequest() {
    std::array<uint8_t, 20> request;
    StunHeader* header = reinterpret_cast<StunHeader*>(request.data());

    header->type = htons(STUN_BINDING_REQUEST);
    header->length = 0;
    header->magic_cookie = htonl(STUN_MAGIC_COOKIE);

    std::random_device rd;
    std::generate_n(header->transaction_id, 12, std::ref(rd));

    return request;
}

// Function to parse STUN Binding Response
std::pair<std::string, int> parseStunBindingResponse(const std::array<uint8_t, 1024>& response, std::size_t length) {
    const StunHeader* header = reinterpret_cast<const StunHeader*>(response.data());

    if (length < sizeof(StunHeader) || ntohs(header->type) != STUN_BINDING_RESPONSE || header->magic_cookie != htonl(STUN_MAGIC_COOKIE)) {
        throw std::runtime_error("Invalid STUN response");
    }

    std::size_t pos = sizeof(StunHeader);
    while (pos + sizeof(StunAttributeHeader) <= length) {
        const StunAttributeHeader* attr_header = reinterpret_cast<const StunAttributeHeader*>(response.data() + pos);
        uint16_t attr_type = ntohs(attr_header->type);
        uint16_t attr_length = ntohs(attr_header->length);

        if (pos + sizeof(StunAttributeHeader) + attr_length > length) {
            throw std::runtime_error("Invalid STUN attribute length");
        }

        if (attr_type == 0x0001 && attr_length >= 8) { // MAPPED-ADDRESS
            const MappedAddress* mapped_addr = reinterpret_cast<const MappedAddress*>(response.data() + pos + sizeof(StunAttributeHeader));

            if (mapped_addr->family == 0x01) { // IPv4
                uint16_t port = ntohs(mapped_addr->port);
                boost::asio::ip::address_v4::bytes_type addr_bytes = mapped_addr->address;

                std::string ip = boost::asio::ip::address_v4(addr_bytes).to_string();
                return {ip, port};
            }
        }

        pos += sizeof(StunAttributeHeader) + attr_length;
    }

    throw std::runtime_error("MAPPED-ADDRESS attribute not found");
}

// Function to get public IP and port using STUN
std::pair<std::string, int> getPublicIPAndPortUsingSTUN(const std::string& stun_server, unsigned int stun_port) {
    try {
        boost::asio::io_context io_context;

        // Create UDP socket
        udp::socket socket(io_context);
        socket.open(udp::v4());

        // Resolve STUN server
        udp::resolver resolver(io_context);
        udp::resolver::query query(udp::v4(), stun_server, std::to_string(stun_port));
        udp::endpoint stun_endpoint = *resolver.resolve(query);

        // Create and send STUN request
        std::array<uint8_t, 20> request = createStunBindingRequest();
        socket.send_to(boost::asio::buffer(request), stun_endpoint);

        // Receive STUN response
        std::array<uint8_t, 1024> response;
        udp::endpoint sender_endpoint;
        std::size_t len = socket.receive_from(boost::asio::buffer(response), sender_endpoint);

        // Parse STUN response to extract public IP and port
        return parseStunBindingResponse(response, len);
    } catch (const std::exception& e) {
        std::cerr << "Exception while getting public IP and port using STUN: " << e.what() << std::endl;
        return {"", 0};
    }
}

// NIF function to get local IP and port
static ERL_NIF_TERM nif_get_local_ip_and_port(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) {
        return enif_make_badarg(env);
    }

    auto result = getLocalIPAndPort();

    ERL_NIF_TERM ip = enif_make_string(env, result.first.c_str(), ERL_NIF_LATIN1);
    ERL_NIF_TERM port = enif_make_int(env, result.second);
    return enif_make_tuple2(env, ip, port);
}

// NIF function to get public IP and port using STUN
static ERL_NIF_TERM nif_get_public_ip_and_port(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    char stun_server[256];
    unsigned int stun_port;
    if (!enif_get_string(env, argv[0], stun_server, sizeof(stun_server), ERL_NIF_LATIN1) ||
        !enif_get_uint(env, argv[1], &stun_port)) {
        return enif_make_badarg(env);
    }

    auto result = getPublicIPAndPortUsingSTUN(stun_server, stun_port);

    ERL_NIF_TERM ip = enif_make_string(env, result.first.c_str(), ERL_NIF_LATIN1);
    ERL_NIF_TERM port = enif_make_int(env, result.second);
    return enif_make_tuple2(env, ip, port);
}

// Define Erlang NIF functions
static ErlNifFunc nif_funcs[] = {
    {"get_local_ip_and_port", 0, nif_get_local_ip_and_port},
    {"get_public_ip_and_port", 2, nif_get_public_ip_and_port}
};

// Erlang NIF initialization
ERL_NIF_INIT(utils_nif, nif_funcs, nullptr, nullptr, nullptr, nullptr)
