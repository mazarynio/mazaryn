// grover_nif.cpp
#include <cmath>
#include <iostream>
#include <numeric>
#include <tuple>
#include <vector>
#include <ctime> // Include ctime for clock
#include "erl_nif.h"
#include "qpp/qpp.h"

using namespace qpp;

// Function to perform Grover's search
static ERL_NIF_TERM grover_search_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    // Ensure we have the correct number of arguments
    if (argc != 3) {
        return enif_make_badarg(env);
    }

    int n, marked, num_queries;

    // Validate and extract arguments
    if (!enif_get_int(env, argv[0], &n) || !enif_get_int(env, argv[1], &marked) || !enif_get_int(env, argv[2], &num_queries)) {
        return enif_make_badarg(env);
    }

    // Validate that n is positive and reasonable
    if (n <= 0) {
        return enif_make_badarg(env);
    }

    // Calculate the size of the database
    idx N = static_cast<idx>(std::llround(std::pow(2, n)));
    if (marked < 0 || marked >= N) {
        return enif_make_badarg(env);
    }

    // Initialize dimensions and subsystems
    std::vector<idx> dims(n, 2); // local dimensions
    std::vector<idx> subsys(n);  // ordered subsystems
    std::iota(std::begin(subsys), std::end(subsys), 0);

    // Initialize the quantum state |0>^⊗n
    ket psi = mket(std::vector<idx>(n, 0));
    psi = (kronpow(gt.H, n) * psi).eval(); // Apply Hadamard gate H⊗n

    // Create the Grover diffusion operator
    cmat G = 2 * prj(psi) - gt.Id(N);

    // Start the timer
    clock_t start = clock();

    // Perform Grover iterations
    for (idx i = 0; i < num_queries; ++i) {
        psi(marked) = -psi(marked); // Oracle application
        psi = (G * psi).eval();     // Diffusion operator application
    }

    // Measure the final state in the computational basis
    auto measured = measure_seq(psi, subsys, dims);
    auto result = std::get<RES>(measured);
    double probability = prod(std::get<PROB>(measured));

    // Calculate elapsed time
    clock_t end = clock();
    double runtime = static_cast<double>(end - start) / CLOCKS_PER_SEC;

    // Prepare the return values
    ERL_NIF_TERM erl_result = enif_make_int(env, multiidx2n(result, dims));
    ERL_NIF_TERM erl_probability = enif_make_double(env, probability);
    ERL_NIF_TERM erl_runtime = enif_make_double(env, runtime);

    // Return the result tuple directly
    return enif_make_tuple3(env, erl_result, erl_probability, erl_runtime);
}

// NIF initialization
static ErlNifFunc nif_funcs[] = {
    {"grover_search", 3, grover_search_nif}
};

ERL_NIF_INIT(grover, nif_funcs, NULL, NULL, NULL, NULL)
