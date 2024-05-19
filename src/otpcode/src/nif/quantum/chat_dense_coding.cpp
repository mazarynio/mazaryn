#include <iostream>
#include <tuple>
#include <vector>
#include <string>
#include <erl_nif.h>
#include <qpp/qpp.h>

using namespace qpp;

idx d = 3; // Qudit dimension

ERL_NIF_TERM send_msg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1 || !enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }

    // Convert Erlang string (list of integers) to C++ string
    unsigned int list_length;
    if (!enif_get_list_length(env, argv[0], &list_length)) {
        return enif_make_badarg(env);
    }

    std::string message;
    ERL_NIF_TERM head, tail;
    tail = argv[0];
    int value;
    for (unsigned int i = 0; i < list_length; i++) {
        if (!enif_get_list_cell(env, tail, &head, &tail) || !enif_get_int(env, head, &value)) {
            return enif_make_badarg(env);
        }
        message += static_cast<char>(value);
    }

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
            // If character is out of range, return the original list
            return argv[0];
        }
    }

    // Convert the decoded message to Erlang list (string)
    ERL_NIF_TERM erl_string = enif_make_list(env, 0);
    for (int i = decoded_message.size() - 1; i >= 0; --i) {
        erl_string = enif_make_list_cell(env, enif_make_int(env, decoded_message[i]), erl_string);
    }

    return erl_string;
}

static ErlNifFunc nif_funcs[] = {
    {"send_msg", 1, send_msg}
};

ERL_NIF_INIT(chat_dense_coding, nif_funcs, NULL, NULL, NULL, NULL);
