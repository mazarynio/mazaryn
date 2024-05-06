#include <erl_nif.h>

ERL_NIF_TERM print_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;

    if (argc != 1 || !enif_inspect_binary(env, argv[0], &input))
        return enif_make_badarg(env);

    //enif_fprintf(stdout, "%.*s\n", input.size, (char*)input.data);

    // Create a new string term from the binary data
    return enif_make_string_len(env, (char*)input.data, input.size, ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] = {
    {"print_string", 1, print_string}
};

ERL_NIF_INIT(print_string, nif_funcs, NULL, NULL, NULL, NULL)
