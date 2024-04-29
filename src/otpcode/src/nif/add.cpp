#include <erl_nif.h>
#include <iostream>

extern "C" {

static ERL_NIF_TERM hello_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x;
    if (!enif_get_int(env, argv[0], &x)) {
        return enif_make_badarg(env);
    }

    int result = x * x;

    return enif_make_int(env, result);
}

// NIF entry function
static ErlNifFunc nif_funcs[] = {
    {"hello_nif", 1, hello_nif}
};

ERL_NIF_INIT(add, nif_funcs, NULL, NULL, NULL, NULL)

}
