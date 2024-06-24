#include "file_nif.h"
#include <filesystem>
#include <cmath>
#include <erl_nif.h>

// File class methods implementation
File::File(const std::string& name_)
    : name(name_), file_size(0) {}

std::string File::getName() const {
    return name;
}

std::size_t File::getFileSize() const {
    return file_size;
}

void File::setFileSize(std::size_t size) {
    file_size = size;
}

std::string File::getHash() const {
    return hash;
}

// NIF function implementation
extern "C" {

static ERL_NIF_TERM upload_file_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char file_name[256]; // Assuming max filename length
    if (!enif_get_string(env, argv[0], file_name, sizeof(file_name), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    char file_path[1024]; // Assuming max file path length
    if (!enif_get_string(env, argv[1], file_path, sizeof(file_path), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    // Instantiate the File object
    File file(file_name); // Use the constructor that sets the name

    // Calculate file size (in bytes)
    std::filesystem::path filePath(file_path);
    std::error_code ec;
    std::uintmax_t size = std::filesystem::file_size(filePath, ec);
    if (ec) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "File not found or inaccessible", ERL_NIF_LATIN1));
    }

    // Update the file size in the File object
    file.setFileSize(size);

    // Convert file size to MB (megabytes) with precision of 3 decimal places
    double size_mb = static_cast<double>(size) / (1024 * 1024);
    size_mb = std::round(size_mb * 1000.0) / 1000.0; // Round to 3 decimal places

    // Create the result tuple {"File_name", "file_size in MB"}
    ERL_NIF_TERM file_name_term = enif_make_string(env, file.getName().c_str(), ERL_NIF_LATIN1);
    ERL_NIF_TERM file_size_mb_term = enif_make_double(env, size_mb);
    ERL_NIF_TERM result_tuple = enif_make_tuple2(env, file_name_term, file_size_mb_term);

    // Return the result tuple
    return result_tuple;
}

// Array of NIFs provided by this module
static ErlNifFunc nif_funcs[] = {
    {"upload_file", 2, upload_file_nif}
};

// Initialize the module
ERL_NIF_INIT(file_nif, nif_funcs, nullptr, nullptr, nullptr, nullptr)

} // end of extern "C"
