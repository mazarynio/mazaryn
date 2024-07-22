#include <iostream>
#include <tuple>
#include <vector>
#include <string>
#include <erl_nif.h>
#include <qpp/qpp.h>

using namespace qpp;

idx d = 3; // Qudit dimension

ERL_NIF_TERM send_msg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1 || !enif_is_binary(env, argv[0])) {
        return enif_make_badarg(env);
    }

    // Extract binary data
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    // Convert binary data to std::string
    std::string message(reinterpret_cast<char*>(bin.data), bin.size);

    // Process the message in segments
    std::string decoded_message;
    for (char& ch : message) {
        // Initialize the maximally entangled state resource
        ket mes_AB = st.mes(d);

        // Define the circuit that measures in the qudit Bell basis
        cmat Bell_AB = adjoint(gt.CTRL(gt.Xd(d), {0}, {1}, 2, d)) * kron(gt.Fd(d), gt.Id(d));

        // Encode the message character
        ket psi_AB = mes_AB;
        idx m_A = static_cast<idx>(ch);
        if (m_A < static_cast<idx>(d * d)) {
            std::vector<idx> midx = qpp::n2multiidx(m_A, {d, d});
            cmat U_A = qpp::powm(qpp::gt.Zd(d), midx[0]) * qpp::powm(qpp::adjoint(qpp::gt.Xd(d)), midx[1]);
            psi_AB = qpp::apply(psi_AB, U_A, {0}, d);

            // Bob measures the joint system in the qudit Bell basis
            psi_AB = apply(psi_AB, Bell_AB, {0, 1}, d);
            auto measured = measure(psi_AB, gt.Id(d * d));
            idx m_B = std::get<RES>(measured);

            // Decode the received message character
            std::vector<idx> midx_B = qpp::n2multiidx(m_B, {d, d});
            std::vector<idx> midx_A = qpp::n2multiidx(static_cast<idx>(ch), {d, d});

            if (midx_B == midx_A) {
                decoded_message += static_cast<char>(m_A);
            } else {
                // If decoding fails, return an error
                return enif_make_atom(env, "error");
            }
        } else {
            // If character is out of range, return the original binary
            return enif_make_binary(env, &bin);
        }
    }

    // Convert the decoded message to Erlang binary
    ERL_NIF_TERM erl_binary;
    unsigned char* erl_binary_data = enif_make_new_binary(env, decoded_message.size(), &erl_binary);
    std::memcpy(erl_binary_data, decoded_message.c_str(), decoded_message.size());

    return erl_binary;
}

static ErlNifFunc nif_funcs[] = {
    {"send_msg", 1, send_msg}
};

ERL_NIF_INIT(chat_dense_coding, nif_funcs, NULL, NULL, NULL, NULL);
