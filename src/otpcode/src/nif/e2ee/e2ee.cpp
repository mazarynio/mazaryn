#include <erl_nif.h>
#include <openssl/evp.h>
#include <openssl/ec.h>
#include <openssl/ecdh.h>
#include <openssl/rand.h>
#include <openssl/err.h>
#include <openssl/x509.h>
#include <vector>
#include <string>
#include <stdexcept>
#include <cstring>

const int KEY_SIZE = 32;  // 256 bits
const int IV_SIZE = 12;   // 96 bits for GCM
const int TAG_SIZE = 16;  // 128 bits for authentication tag

void handle_openssl_error() {
    char err_buf[256];
    ERR_error_string_n(ERR_get_error(), err_buf, sizeof(err_buf));
    throw std::runtime_error(std::string("OpenSSL error: ") + err_buf);
}

EVP_PKEY* generate_ecdh_key() {
    EVP_PKEY_CTX *pctx = EVP_PKEY_CTX_new_id(EVP_PKEY_EC, nullptr);
    EVP_PKEY *pkey = nullptr;
    if (EVP_PKEY_keygen_init(pctx) <= 0 ||
        EVP_PKEY_CTX_set_ec_paramgen_curve_nid(pctx, NID_X9_62_prime256v1) <= 0 ||
        EVP_PKEY_keygen(pctx, &pkey) <= 0) {
        EVP_PKEY_CTX_free(pctx);
        handle_openssl_error();
    }
    EVP_PKEY_CTX_free(pctx);
    return pkey;
}

std::vector<unsigned char> derive_shared_secret(EVP_PKEY* private_key, EVP_PKEY* peer_public_key) {
    EVP_PKEY_CTX *ctx = EVP_PKEY_CTX_new(private_key, nullptr);
    std::vector<unsigned char> shared_secret(KEY_SIZE);
    size_t secret_len = shared_secret.size();
    
    if (EVP_PKEY_derive_init(ctx) <= 0 ||
        EVP_PKEY_derive_set_peer(ctx, peer_public_key) <= 0 ||
        EVP_PKEY_derive(ctx, shared_secret.data(), &secret_len) <= 0) {
        EVP_PKEY_CTX_free(ctx);
        handle_openssl_error();
    }
    
    EVP_PKEY_CTX_free(ctx);
    shared_secret.resize(secret_len);
    return shared_secret;
}

std::vector<unsigned char> encrypt(const std::string& plaintext, const std::vector<unsigned char>& key) {
    std::vector<unsigned char> ciphertext(plaintext.size() + IV_SIZE + TAG_SIZE);
    unsigned char* iv = ciphertext.data();
    unsigned char* tag = ciphertext.data() + IV_SIZE + plaintext.size();

    if (RAND_bytes(iv, IV_SIZE) != 1) {
        handle_openssl_error();
    }

    EVP_CIPHER_CTX* ctx = EVP_CIPHER_CTX_new();
    if (!ctx) handle_openssl_error();

    if (EVP_EncryptInit_ex(ctx, EVP_aes_256_gcm(), nullptr, key.data(), iv) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        handle_openssl_error();
    }

    int len = 0, ciphertext_len = 0;
    if (EVP_EncryptUpdate(ctx, ciphertext.data() + IV_SIZE, &len, 
                          reinterpret_cast<const unsigned char*>(plaintext.data()), 
                          plaintext.size()) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        handle_openssl_error();
    }
    ciphertext_len = len;

    if (EVP_EncryptFinal_ex(ctx, ciphertext.data() + IV_SIZE + len, &len) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        handle_openssl_error();
    }
    ciphertext_len += len;

    if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_GET_TAG, TAG_SIZE, tag) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        handle_openssl_error();
    }

    EVP_CIPHER_CTX_free(ctx);
    ciphertext.resize(IV_SIZE + ciphertext_len + TAG_SIZE);
    return ciphertext;
}

std::string decrypt(const std::vector<unsigned char>& ciphertext, const std::vector<unsigned char>& key) {
    if (ciphertext.size() < IV_SIZE + TAG_SIZE) {
        throw std::runtime_error("Ciphertext too short");
    }

    const unsigned char* iv = ciphertext.data();
    const unsigned char* tag = ciphertext.data() + ciphertext.size() - TAG_SIZE;
    size_t ciphertext_len = ciphertext.size() - IV_SIZE - TAG_SIZE;

    std::vector<unsigned char> plaintext(ciphertext_len);

    EVP_CIPHER_CTX* ctx = EVP_CIPHER_CTX_new();
    if (!ctx) handle_openssl_error();

    if (EVP_DecryptInit_ex(ctx, EVP_aes_256_gcm(), nullptr, key.data(), iv) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        handle_openssl_error();
    }

    int len = 0, plaintext_len = 0;
    if (EVP_DecryptUpdate(ctx, plaintext.data(), &len, 
                          ciphertext.data() + IV_SIZE, ciphertext_len) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        handle_openssl_error();
    }
    plaintext_len = len;

    if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_TAG, TAG_SIZE, (void*)tag) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        handle_openssl_error();
    }

    if (EVP_DecryptFinal_ex(ctx, plaintext.data() + len, &len) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        handle_openssl_error();
    }
    plaintext_len += len;

    EVP_CIPHER_CTX_free(ctx);
    return std::string(plaintext.begin(), plaintext.begin() + plaintext_len);
}

std::vector<unsigned char> sign_message(const std::string& message, EVP_PKEY* private_key) {
    EVP_MD_CTX *md_ctx = EVP_MD_CTX_new();
    std::vector<unsigned char> signature(EVP_PKEY_size(private_key));
    size_t sig_len = signature.size();

    if (EVP_DigestSignInit(md_ctx, nullptr, nullptr, nullptr, private_key) <= 0 ||
        EVP_DigestSign(md_ctx, signature.data(), &sig_len,
                       reinterpret_cast<const unsigned char*>(message.data()), message.size()) <= 0) {
        EVP_MD_CTX_free(md_ctx);
        handle_openssl_error();
    }

    EVP_MD_CTX_free(md_ctx);
    signature.resize(sig_len);
    return signature;
}

bool verify_signature(const std::string& message, const std::vector<unsigned char>& signature, EVP_PKEY* public_key) {
    EVP_MD_CTX *md_ctx = EVP_MD_CTX_new();
    int ret = EVP_DigestVerifyInit(md_ctx, nullptr, nullptr, nullptr, public_key);
    if (ret <= 0) {
        EVP_MD_CTX_free(md_ctx);
        handle_openssl_error();
    }

    ret = EVP_DigestVerify(md_ctx, signature.data(), signature.size(),
                           reinterpret_cast<const unsigned char*>(message.data()), message.size());
    EVP_MD_CTX_free(md_ctx);

    if (ret < 0) {
        handle_openssl_error();
    }
    return ret == 1;
}

// NIF function declarations
extern "C" {
    static ERL_NIF_TERM generate_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM derive_secret(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM encrypt_message(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM decrypt_message(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM sign(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM verify(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
}

// NIF function implementations
static ERL_NIF_TERM generate_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    try {
        EVP_PKEY* key = generate_ecdh_key();
        
        // Serialize private key
        unsigned char* priv_buf = nullptr;
        int priv_len = i2d_PrivateKey(key, &priv_buf);
        if (priv_len <= 0) {
            EVP_PKEY_free(key);
            return enif_make_string(env, "Failed to serialize private key", ERL_NIF_LATIN1);
        }
        
        // Serialize public key
        unsigned char* pub_buf = nullptr;
        int pub_len = i2d_PUBKEY(key, &pub_buf);
        if (pub_len <= 0) {
            OPENSSL_free(priv_buf);
            EVP_PKEY_free(key);
            return enif_make_string(env, "Failed to serialize public key", ERL_NIF_LATIN1);
        }
        
        // Create binary terms
        ERL_NIF_TERM priv_term, pub_term;
        unsigned char* priv_data = enif_make_new_binary(env, priv_len, &priv_term);
        unsigned char* pub_data = enif_make_new_binary(env, pub_len, &pub_term);
        
        memcpy(priv_data, priv_buf, priv_len);
        memcpy(pub_data, pub_buf, pub_len);
        
        OPENSSL_free(priv_buf);
        OPENSSL_free(pub_buf);
        EVP_PKEY_free(key);
        
        return enif_make_tuple2(env, priv_term, pub_term);
    } catch (const std::exception& e) {
        return enif_make_string(env, e.what(), ERL_NIF_LATIN1);
    }
}


static ERL_NIF_TERM derive_secret(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary private_key_bin, public_key_bin;
    if (!enif_inspect_binary(env, argv[0], &private_key_bin) || !enif_inspect_binary(env, argv[1], &public_key_bin)) {
        return enif_make_badarg(env);
    }

    try {
        const unsigned char* p = private_key_bin.data;
        EVP_PKEY* private_key = d2i_PrivateKey(EVP_PKEY_EC, nullptr, &p, private_key_bin.size);
        if (!private_key) handle_openssl_error();

        p = public_key_bin.data;
        EVP_PKEY* public_key = d2i_PUBKEY(nullptr, &p, public_key_bin.size);
        if (!public_key) {
            EVP_PKEY_free(private_key);
            handle_openssl_error();
        }

        std::vector<unsigned char> secret = derive_shared_secret(private_key, public_key);

        EVP_PKEY_free(private_key);
        EVP_PKEY_free(public_key);

        ERL_NIF_TERM binary_term;
        unsigned char* binary_data = enif_make_new_binary(env, secret.size(), &binary_term);
        memcpy(binary_data, secret.data(), secret.size());
        return binary_term;  // Return just the binary term
    } catch (const std::exception& e) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, e.what(), ERL_NIF_LATIN1));
    }
}

static ERL_NIF_TERM encrypt_message(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary plaintext_bin, key_bin;
    if (!enif_inspect_binary(env, argv[0], &plaintext_bin) || !enif_inspect_binary(env, argv[1], &key_bin)) {
        return enif_make_badarg(env);
    }

    try {
        std::string plaintext(reinterpret_cast<char*>(plaintext_bin.data), plaintext_bin.size);
        std::vector<unsigned char> key(key_bin.data, key_bin.data + key_bin.size);

        std::vector<unsigned char> ciphertext = encrypt(plaintext, key);

        ERL_NIF_TERM binary_term;
        unsigned char* binary_data = enif_make_new_binary(env, ciphertext.size(), &binary_term);
        memcpy(binary_data, ciphertext.data(), ciphertext.size());
        return binary_term;
    } catch (const std::exception& e) {
        return enif_make_string(env, e.what(), ERL_NIF_LATIN1);
    }
}

static ERL_NIF_TERM decrypt_message(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary ciphertext_bin, key_bin;
    if (!enif_inspect_binary(env, argv[0], &ciphertext_bin) || !enif_inspect_binary(env, argv[1], &key_bin)) {
        return enif_make_badarg(env);
    }

    try {
        std::vector<unsigned char> ciphertext(ciphertext_bin.data, ciphertext_bin.data + ciphertext_bin.size);
        std::vector<unsigned char> key(key_bin.data, key_bin.data + key_bin.size);

        std::string plaintext = decrypt(ciphertext, key);

        ERL_NIF_TERM binary_term;
        unsigned char* binary_data = enif_make_new_binary(env, plaintext.size(), &binary_term);
        memcpy(binary_data, plaintext.data(), plaintext.size());
        return binary_term;
    } catch (const std::exception& e) {
        return enif_make_string(env, e.what(), ERL_NIF_LATIN1);
    }
}

static ERL_NIF_TERM sign(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary message_bin, private_key_bin;
    if (!enif_inspect_binary(env, argv[0], &message_bin) || !enif_inspect_binary(env, argv[1], &private_key_bin)) {
        return enif_make_badarg(env);
    }

    try {
        std::string message(reinterpret_cast<char*>(message_bin.data), message_bin.size);

        const unsigned char* p = private_key_bin.data;
        EVP_PKEY* private_key = d2i_PrivateKey(EVP_PKEY_EC, nullptr, &p, private_key_bin.size);
        if (!private_key) handle_openssl_error();

        std::vector<unsigned char> signature = sign_message(message, private_key);

        EVP_PKEY_free(private_key);

        ERL_NIF_TERM binary_term;
        unsigned char* binary_data = enif_make_new_binary(env, signature.size(), &binary_term);
        memcpy(binary_data, signature.data(), signature.size());
        return binary_term;
    } catch (const std::exception& e) {
        return enif_make_string(env, e.what(), ERL_NIF_LATIN1);
    }
}


static ERL_NIF_TERM verify(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary message_bin, signature_bin, public_key_bin;
    if (!enif_inspect_binary(env, argv[0], &message_bin) || !enif_inspect_binary(env, argv[1], &signature_bin) || !enif_inspect_binary(env, argv[2], &public_key_bin)) {
        return enif_make_badarg(env);
    }

    try {
        std::string message(reinterpret_cast<char*>(message_bin.data), message_bin.size);
        std::vector<unsigned char> signature(signature_bin.data, signature_bin.data + signature_bin.size);

        const unsigned char* p = public_key_bin.data;
        EVP_PKEY* public_key = d2i_PUBKEY(nullptr, &p, public_key_bin.size);
        if (!public_key) handle_openssl_error();

        bool valid = verify_signature(message, signature, public_key);

        EVP_PKEY_free(public_key);

        return enif_make_tuple2(env, enif_make_atom(env, valid ? "ok" : "error"), enif_make_atom(env, valid ? "true" : "false"));
    } catch (const std::exception& e) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, e.what(), ERL_NIF_LATIN1));
    }
}

// NIF function definitions
static ErlNifFunc nif_funcs[] = {
    {"generate_key", 0, generate_key},
    {"derive_secret", 2, derive_secret},
    {"encrypt_message", 2, encrypt_message},
    {"decrypt_message", 2, decrypt_message},
    {"sign", 2, sign},
    {"verify", 3, verify}
};

// NIF initialization
ERL_NIF_INIT(e2ee, nif_funcs, nullptr, nullptr, nullptr, nullptr) 
