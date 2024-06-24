#include "hash_nif.h"
#include <openssl/evp.h>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <erl_nif.h>

std::string Hash::computeSHA256(const std::string& filePath) {
    std::ifstream file(filePath, std::ios::binary);
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + filePath);
    }

    EVP_MD_CTX* context = EVP_MD_CTX_new();
    if (!context) {
        throw std::runtime_error("Could not create EVP_MD_CTX");
    }

    const EVP_MD* md = EVP_sha256();
    if (!EVP_DigestInit_ex(context, md, nullptr)) {
        EVP_MD_CTX_free(context);
        throw std::runtime_error("Could not initialize digest context");
    }

    char buffer[4096];
    while (file.good()) {
        file.read(buffer, sizeof(buffer));
        if (!EVP_DigestUpdate(context, buffer, file.gcount())) {
            EVP_MD_CTX_free(context);
            throw std::runtime_error("Could not update digest");
        }
    }

    unsigned char hash[EVP_MAX_MD_SIZE];
    unsigned int lengthOfHash = 0;
    if (!EVP_DigestFinal_ex(context, hash, &lengthOfHash)) {
        EVP_MD_CTX_free(context);
        throw std::runtime_error("Could not finalize digest");
    }

    EVP_MD_CTX_free(context);

    std::ostringstream result;
    result << std::hex << std::setfill('0');
    for (unsigned int i = 0; i < lengthOfHash; ++i) {
        result << std::setw(2) << static_cast<int>(hash[i]);
    }

    return result.str();
}

static ERL_NIF_TERM nif_compute_sha256(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    char filePath[4096];
    if (!enif_get_string(env, argv[0], filePath, sizeof(filePath), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    try {
        std::string hash = Hash::computeSHA256(filePath);
        return enif_make_string(env, hash.c_str(), ERL_NIF_LATIN1);
    } catch (const std::exception& e) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, e.what(), ERL_NIF_LATIN1));
    }
}

static ErlNifFunc nif_funcs[] = {
    {"compute_sha256", 1, nif_compute_sha256}
};

ERL_NIF_INIT(hash_nif, nif_funcs, nullptr, nullptr, nullptr, nullptr)
