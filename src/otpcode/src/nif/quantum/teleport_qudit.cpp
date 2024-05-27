#include <cmath>
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

// Function to perform Qudit teleportation
static ERL_NIF_TERM qudit_teleport(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    int d_int;
    if (!enif_get_int(env, argv[0], &d_int)) {
        return enif_make_badarg(env);
    }

    // Ensure d is non-negative and convert to idx type
    if (d_int < 0) {
        return enif_make_badarg(env);
    }
    idx d = static_cast<idx>(d_int);

    // Qudit teleportation
    ket mes_AB = st.mes(d); // maximally entangled state resource

    // Circuit used to measure in the qudit Bell basis
    cmat Bell_aA = kron(adjoint(gt.Fd(d)), gt.Id(d)) *
                   gt.CTRL(adjoint(gt.Xd(d)), {0}, {1}, 2, d);

    ket psi_a = randket(d); // random qudit state
    ket input_aAB = kron(psi_a, mes_AB); // joint input state aAB

    // Output before measurement
    ket output_aAB = apply(input_aAB, Bell_aA, {0, 1}, d);

    // Measure on aA
    auto [m_aA, probs_aA, states_B] =
        measure(output_aAB, gt.Id(d * d), {0, 1}, d);

    std::vector<idx> midx = n2multiidx(m_aA, {d, d});
    ket output_B = states_B[m_aA];

    // Perform the correction on B
    cmat correction_B =
        powm(gt.Zd(d), midx[0]) * powm(adjoint(gt.Xd(d)), midx[1]);
    ket psi_B = correction_B * output_B;

    // Verification
    double norm_diff = norm(psi_B - psi_a);

    // Convert Eigen vectors to strings
    std::string initial_state_str = eigen_to_string(psi_a);
    std::string final_state_str = eigen_to_string(psi_B);

    // Prepare the results to be returned to Erlang
    ERL_NIF_TERM erl_initial_state = enif_make_string(env, initial_state_str.c_str(), ERL_NIF_LATIN1);
    ERL_NIF_TERM erl_final_state = enif_make_string(env, final_state_str.c_str(), ERL_NIF_LATIN1);
    ERL_NIF_TERM erl_norm_diff = enif_make_double(env, norm_diff);
    ERL_NIF_TERM erl_correction = enif_make_tuple2(env, enif_make_int(env, midx[0]), enif_make_int(env, (d - midx[1]) % d));

    return enif_make_tuple4(env, erl_initial_state, erl_final_state, erl_norm_diff, erl_correction);
}

// NIF initialization
static ErlNifFunc nif_funcs[] = {
    {"qudit_teleport", 1, qudit_teleport}
};

ERL_NIF_INIT(teleport_qudit, nif_funcs, NULL, NULL, NULL, NULL)
