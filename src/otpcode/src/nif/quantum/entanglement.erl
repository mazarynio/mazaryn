-module(entanglement).
-export([init/0, density_matrix_entanglement/1, state_entanglement/1]).
-on_load(init/0).
-define(LIBNAME, "libentanglement").
-include("records.hrl").
init() ->
    erlang:load_nif(filename:join(?PrivDir, ?LIBNAME), 0).

%% Not work well as expected
%% 1> Rho = [0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.8, 
%% 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0].
%% entanglement:density_matrix_entanglement(Rho).
density_matrix_entanglement(_RhoList) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).

%% Psi = [0.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.6].
%% entanglement:state_entanglement(Psi).
state_entanglement(_PsiList) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
