#include <iostream>
#include <tuple>
#include <vector>
#include <ctime>
#include "erl_nif.h"
#include "qpp/qpp.h"

using namespace qpp;

// Function to convert Eigen vectors/matrices to string
std::string eigen_to_string(const Eigen::MatrixXcd& mat) {
    std::stringstream ss;
    ss << mat;
    return ss.str();
}

// Function to perform Qubit teleportation
static ERL_NIF_TERM qubit_teleport(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 4) {
        return enif_make_badarg(env);
    }

    double real0, imag0, real1, imag1;
    if (!enif_get_double(env, argv[0], &real0) ||
        !enif_get_double(env, argv[1], &imag0) ||
        !enif_get_double(env, argv[2], &real1) ||
        !enif_get_double(env, argv[3], &imag1)) {
        return enif_make_badarg(env);
    }

    // Create the initial state based on user input
    ket psi_a(2);
    psi_a << std::complex<double>(real0, imag0), std::complex<double>(real1, imag1);

    // Normalize the state vector
    psi_a = psi_a / norm(psi_a);

    // the entangled resource
    ket phi_AB = st.b00;

    // global input state
    ket input_aAB = kron(psi_a, phi_AB);

    // apply a CNOT on qubits 'AB' followed by an H on qubit 'a'
    input_aAB = applyCTRL(input_aAB, gt.X, {0}, {1});
    input_aAB = apply(input_aAB, gt.H, {0});

    // measure the aA part
    auto [ms_aA, probs_aA, psi_B] = measure_seq(input_aAB, {0, 1});

    // measurement results
    idx z = ms_aA[0];
    idx x = ms_aA[1];

    // probability of obtaining the measurement results x and z
    realT p = prod(probs_aA);

    // the output state (before correction)
    ket psi_B_before_correction = psi_B;

    // perform the correction on B
    psi_B = powm(gt.Z, z) * powm(gt.X, x) * psi_B;

    // Convert Eigen vectors to strings
    std::string initial_state_str = eigen_to_string(psi_a);
    std::string final_state_str = eigen_to_string(psi_B);

    // Calculate norm difference for verification
    double norm_diff = norm(psi_B - psi_a);

    // Prepare the results to be returned to Erlang
    ERL_NIF_TERM erl_initial_state = enif_make_string(env, initial_state_str.c_str(), ERL_NIF_LATIN1);
    ERL_NIF_TERM erl_final_state = enif_make_string(env, final_state_str.c_str(), ERL_NIF_LATIN1);
    ERL_NIF_TERM erl_norm_diff = enif_make_double(env, norm_diff);
    ERL_NIF_TERM erl_measurement = enif_make_tuple2(env, enif_make_int(env, x), enif_make_int(env, z));
    ERL_NIF_TERM erl_probability = enif_make_double(env, p);

    return enif_make_tuple5(env, erl_initial_state, erl_final_state, erl_norm_diff, erl_measurement, erl_probability);
}

// NIF initialization
static ErlNifFunc nif_funcs[] = {
    {"qubit_teleport", 4, qubit_teleport}
};

ERL_NIF_INIT(teleport_qubit, nif_funcs, NULL, NULL, NULL, NULL)
