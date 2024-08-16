#include <iostream>
#include <string>
#include <erl_nif.h>

// Function to get the device info
std::string device_info() {
#if defined(_WIN32) || defined(_WIN64)
    return "Windows";
#elif defined(__APPLE__)
    #include <TargetConditionals.h>
    #if TARGET_OS_IOS
        return "iOS";
    #elif TARGET_OS_MAC
        return "macOS";
    #else
        return "Apple, but not iOS or macOS";
    #endif
#elif defined(__linux__)
    return "Linux";
#elif defined(__ANDROID__)
    return "Android";
#else
    return "Unknown";
#endif
}

// NIF wrapper function
static ERL_NIF_TERM nif_device_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv) {
    // Get the device info
    std::string info = device_info();

    // Convert std::string to Erlang term
    return enif_make_string(env, info.c_str(), ERL_NIF_LATIN1);
}

// List of NIF functions
static ErlNifFunc nif_funcs[] = {
    {"nif_device_info", 0, nif_device_info}
};

// Load function for the NIF
ERL_NIF_INIT(device, nif_funcs, nullptr, nullptr, nullptr, nullptr) 

