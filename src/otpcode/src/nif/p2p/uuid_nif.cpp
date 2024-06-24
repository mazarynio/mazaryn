#include <erl_nif.h>
#include <random>
#include <iomanip>
#include <sstream>
#include <string>
#include "uuid_nif.h"

// Function to generate a UUID
std::string generate_uuid(int length) {
    // Create a cryptographically secure random device
    std::random_device rd("/dev/urandom");  // Linux specific, adjust for other platforms
    std::mt19937 gen(rd());

    // Create a distribution for generating random bytes
    std::uniform_int_distribution<int> dis(0, 255);

    // Generate the specified number of random bytes
    std::stringstream ss;
    ss << std::hex << std::setfill('0');
    for (int i = 0; i < length; ++i) {
        int byte = dis(gen);
        ss << std::setw(2) << byte;
    }

    return ss.str();
}

// NIF function to generate UUID
static ERL_NIF_TERM nif_generate_uuid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    int length;
    if (!enif_get_int(env, argv[0], &length)) {
        return enif_make_badarg(env);
    }

    std::string uuid = generate_uuid(length);
    return enif_make_string(env, uuid.c_str(), ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] = {
    {"generate", 1, nif_generate_uuid}
};

extern "C" {
    ERL_NIF_INIT(uuid_nif, nif_funcs, NULL, NULL, NULL, NULL)
}
