#include <iostream>
#include <vector>
#include <complex>
#include <erl_nif.h>
#include <qpp/qpp.h>

using namespace qpp;

// Helper function to convert a vector of doubles to an Erlang list
ERL_NIF_TERM make_double_list(ErlNifEnv* env, const std::vector<double>& vec) {
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (auto it = vec.rbegin(); it != vec.rend(); ++it) {
        list = enif_make_list_cell(env, enif_make_double(env, *it), list);
    }
    return list;
}

// Helper function to convert Erlang list to std::vector<double>
bool get_double_list(ErlNifEnv* env, ERL_NIF_TERM list, std::vector<double>& vec) {
    unsigned len;
    if (!enif_get_list_length(env, list, &len)) {
        return false;
    }
    vec.resize(len);
    ERL_NIF_TERM head, tail;
    for (unsigned i = 0; i < len; ++i) {
        if (!enif_get_list_cell(env, list, &head, &tail) || !enif_get_double(env, head, &vec[i])) {
            return false;
        }
        list = tail;
    }
    return true;
}

// Function to calculate entanglement measures for a given density matrix
ERL_NIF_TERM density_matrix_entanglement(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1 || !enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }

    std::vector<double> rho_elements;
    if (!get_double_list(env, argv[0], rho_elements)) {
        return enif_make_badarg(env);
    }

    if (rho_elements.size() != 32) { // 4x4 complex matrix (16 real parts + 16 imaginary parts)
        return enif_make_badarg(env);
    }

    std::vector<std::complex<double>> rho_complex_elements(16);
    for (int i = 0; i < 16; ++i) {
        rho_complex_elements[i] = std::complex<double>(rho_elements[2 * i], rho_elements[2 * i + 1]);
    }

    cmat rho = Eigen::Map<cmat>(rho_complex_elements.data(), 4, 4);

    double rho_concurrence = concurrence(rho);
    double rho_negativity = negativity(rho, {2, 2});
    double rho_lognegativity = lognegativity(rho, {2, 2});

    return enif_make_tuple3(env,
        enif_make_double(env, rho_concurrence),
        enif_make_double(env, rho_negativity),
        enif_make_double(env, rho_lognegativity)
    );
}

// Function to calculate entanglement measures for a given state
ERL_NIF_TERM state_entanglement(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1 || !enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }

    std::vector<double> psi_elements;
    if (!get_double_list(env, argv[0], psi_elements)) {
        return enif_make_badarg(env);
    }

    if (psi_elements.size() != 8) { // 4 complex elements (4 real parts + 4 imaginary parts)
        return enif_make_badarg(env);
    }

    std::vector<std::complex<double>> psi_complex_elements(4);
    for (int i = 0; i < 4; ++i) {
        psi_complex_elements[i] = std::complex<double>(psi_elements[2 * i], psi_elements[2 * i + 1]);
    }

    ket psi = Eigen::Map<ket>(psi_complex_elements.data(), 4);

    // Calculate entanglement measures
    double psi_entanglement = entanglement(psi, {2, 2});
    double psi_concurrence = concurrence(prj(psi));
    double psi_gconcurrence = gconcurrence(psi);

    // Calculate Schmidt coefficients and probabilities
    auto schmidt_coeffs = schmidtcoeffs(psi, {2, 2});
    auto schmidt_probs = schmidtprobs(psi, {2, 2});

    // Convert Eigen::VectorXd to std::vector<double>
    std::vector<double> schmidt_coeffs_vec(schmidt_coeffs.data(), schmidt_coeffs.data() + schmidt_coeffs.size());
    std::vector<double> schmidt_probs_vec(schmidt_probs.data(), schmidt_probs.data() + schmidt_probs.size());

    // Create Erlang lists from vectors
    ERL_NIF_TERM schmidt_coeffs_list = make_double_list(env, schmidt_coeffs_vec);
    ERL_NIF_TERM schmidt_probs_list = make_double_list(env, schmidt_probs_vec);

    // Return a tuple containing all calculated values
    return enif_make_tuple5(env,
        enif_make_double(env, psi_entanglement),
        enif_make_double(env, psi_concurrence),
        enif_make_double(env, psi_gconcurrence),
        schmidt_coeffs_list,
        schmidt_probs_list
    );
}

static ErlNifFunc nif_funcs[] = {
    {"density_matrix_entanglement", 1, density_matrix_entanglement},
    {"state_entanglement", 1, state_entanglement}
};

ERL_NIF_INIT(entanglement, nif_funcs, NULL, NULL, NULL, NULL);
