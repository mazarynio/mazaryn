#include <cmath>
#include <iostream>
#include <vector>
#include <tuple>
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

// Function to perform Quantum Fourier Transform
static ERL_NIF_TERM qft_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    // Get the input list of qubits from Erlang
    unsigned int list_length;
    if (!enif_get_list_length(env, argv[0], &list_length)) {
        return enif_make_badarg(env);
    }

    std::vector<idx> qubits(list_length);
    ERL_NIF_TERM list = argv[0];
    ERL_NIF_TERM head;
    int qubit;
    for (unsigned int i = 0; i < list_length; ++i) {
        if (!enif_get_list_cell(env, list, &head, &list) || !enif_get_int(env, head, &qubit)) {
            return enif_make_badarg(env);
        }
        qubits[i] = static_cast<idx>(qubit);
    }

    ket psi = mket(qubits);
    ket result = psi;

    idx n = qubits.size();                                   // number of qubits
    auto D = static_cast<idx>(std::llround(std::pow(2, n))); // dimension 2^n

    std::stringstream ss;
    ss << ">> QFT on n = " << n << " qubits. ";
    ss << "The sequence of applied gates is:\n";
    for (idx i = 0; i < n; ++i) {
        ss << "H" << i << " ";
        result = apply(result, gt.H, {i}); // apply Hadamard on qubit 'i'
        // apply controlled rotations
        for (idx j = 2; j <= n - i; ++j) {
            cmat Rj(2, 2);
            auto pow_j = static_cast<idx>(std::llround(std::pow(2, j)));
            Rj << 1, 0, 0, omega(pow_j);
            result = applyCTRL(result, Rj, {static_cast<idx>(i + j - 1)},
                               {static_cast<idx>(i)});
            ss << "R" << j << "(" << i + j - 1 << ", " << i << ") ";
        }
        ss << '\n';
    }

    // we have the qubits in reversed order, we must swap them
    for (idx i = 0; i < n / 2; ++i) {
        ss << "SWAP(" << i << ", " << n - i - 1 << ")\n";
        result = apply(result, gt.SWAP, {i, static_cast<idx>(n - i - 1)});
    }

    // check that we got the Fourier transform, compute the norm difference
    double norm_diff = -1;
    if (n < 14) { // otherwise, not enough memory for computing gt.Fd(D) * psi
        norm_diff = norm(result - gt.Fd(D) * psi);
        ss << ">> Norm difference: " << norm_diff << '\n';
    }

    std::string result_str = eigen_to_string(result);
    ERL_NIF_TERM erl_result_str = enif_make_string(env, result_str.c_str(), ERL_NIF_LATIN1);
    ERL_NIF_TERM erl_steps_str = enif_make_string(env, ss.str().c_str(), ERL_NIF_LATIN1);
    ERL_NIF_TERM erl_norm_diff = enif_make_double(env, norm_diff);

    return enif_make_tuple3(env, erl_result_str, erl_steps_str, erl_norm_diff);
}

// NIF initialization
static ErlNifFunc nif_funcs[] = {
    {"qft_nif", 1, qft_nif}
};

ERL_NIF_INIT(qft, nif_funcs, NULL, NULL, NULL, NULL)
