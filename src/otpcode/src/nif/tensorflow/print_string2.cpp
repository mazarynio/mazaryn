#include <erl_nif.h>

ERL_NIF_TERM print_string2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    ErlNifUInt64 len;
    char input[256]; // Initialize a buffer to store the input string

    // Retrieve the string input from the Erlang term
    if (argc != 1 || !enif_get_string(env, argv[0], input, sizeof(input), ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    // Print the received string
    //enif_fprintf(stdout, "Received string: %s\n", input);

    // You can perform further processing with the input string here

    // Return a response term if needed
    return enif_make_string(env, input, ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] = {
    {"print_string2", 1, print_string2}
};

ERL_NIF_INIT(print_string2, nif_funcs, NULL, NULL, NULL, NULL)
