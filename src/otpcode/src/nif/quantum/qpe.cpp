#include <cmath>
#include <iostream>
#include <numeric>
#include <tuple>
#include <vector>
#include <erl_nif.h>
#include <qpp/qpp.h>

using namespace qpp;

ERL_NIF_TERM estimation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1 || !enif_is_number(env, argv[0])) {
        return enif_make_badarg(env);
    }

    double theta;
    if (!enif_get_double(env, argv[0], &theta)) {
        return enif_make_badarg(env);
    }

    idx nq_c = 3; // number of counting qubits
    idx nq_a = 1; // number of ancilla qubits
    idx nq = nq_c + nq_a; // total number of qubits
    ket psi = mket(std::vector<idx>(nq, 0)); // |0>^\otimes n

    cmat U(2, 2); // initialize a unitary operator
    U << 1, 0, 0, std::exp(2 * pi * 1_i * theta);

    ket result = psi;
    std::vector<idx> counting_qubits(nq_c);
    std::iota(counting_qubits.begin(), counting_qubits.end(), 0);
    std::vector<idx> ancilla(nq_a);
    std::iota(ancilla.begin(), ancilla.end(), nq_c);

    for (idx i = 0; i < static_cast<idx>(counting_qubits.size()); ++i) {
        result = apply(result, gt.H, {i});
    }
    result = apply(result, gt.X, ancilla);

    idx powerU = 1;
    for (idx i = 0; i < nq_c; ++i) {
        result = applyCTRL(result, U, {static_cast<idx>(nq_c - i - 1)}, ancilla);
        U = powm(U, 2);
        powerU *= 2;
    }

    result = applyTFQ(result, counting_qubits);

    auto measured = measure_seq(result, {counting_qubits});
    auto res = std::get<RES>(measured);

    idx decimal = multiidx2n(res, std::vector<idx>(counting_qubits.size(), 2));

    auto theta_e = static_cast<realT>(decimal) / static_cast<realT>(std::pow(2, nq_c));

    return enif_make_double(env, theta_e);
}

static ErlNifFunc nif_funcs[] = {
    {"estimation", 1, estimation}
};

ERL_NIF_INIT(qpe, nif_funcs, NULL, NULL, NULL, NULL);
