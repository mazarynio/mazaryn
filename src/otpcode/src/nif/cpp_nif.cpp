#include <erl_nif.h>
#include <iostream>
#include <string>

extern "C" {
    static ERL_NIF_TERM call_cpp_code(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
    {
        std::cout << "This work well... " << "\n";

        return enif_make_atom(env, "done");
    }

    // NIF entry function
    static ErlNifFunc nif_funcs[] = {
        {"call_cpp_code", 0, call_cpp_code}
    };

    ERL_NIF_INIT(cpp_nif, nif_funcs, NULL, NULL, NULL, NULL)
}