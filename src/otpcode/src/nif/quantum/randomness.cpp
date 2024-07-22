#include <iostream>
#include <vector>
#include <string>
#include <erl_nif.h>
#include <qpp/qpp.h>

using namespace qpp;

// Function to generate a random ket of a specified dimension
ERL_NIF_TERM generate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1 || !enif_is_number(env, argv[0])) {
        return enif_make_badarg(env);
    }

    int dimension;
    if (!enif_get_int(env, argv[0], &dimension)) {
        return enif_make_badarg(env);
    }

    if (dimension <= 0) {
        return enif_make_badarg(env);
    }

    // Generate a random ket of the specified dimension
    ket rket = randket(dimension);

    // Convert the ket to a list of doubles (real and imaginary parts)
    ERL_NIF_TERM erl_rket = enif_make_list(env, 0);
    for (int i = dimension - 1; i >= 0; --i) {
        ERL_NIF_TERM real_part = enif_make_double(env, std::real(rket[i]));
        ERL_NIF_TERM imag_part = enif_make_double(env, std::imag(rket[i]));
        ERL_NIF_TERM complex_number = enif_make_tuple2(env, real_part, imag_part);
        erl_rket = enif_make_list_cell(env, complex_number, erl_rket);
    }

    return erl_rket;
}

static ErlNifFunc nif_funcs[] = {
    {"generate", 1, generate}
};

ERL_NIF_INIT(randomness, nif_funcs, NULL, NULL, NULL, NULL);
