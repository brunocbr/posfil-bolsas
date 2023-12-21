%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Distribuição de Bolsas - PPG Filosofia PUC-SP
%
% Autor: Bruno Loureiro Conte
% GitHub: https://github.com/brunocbr/posfil-bolsas
%
% Descrição: Este arquivo contém o código-fonte do programa desenvolvido para
% realizar a distribuição de bolsas do Programa de Pós-Graduação em Filosofia da
% PUC-SP. O código implementa critérios como nível acadêmico, cota racial,
% agência financiadora e modalidade para distribuir as bolsas entre os
% candidatos de forma justa e transparente.
%
% Este programa é parte integrante do projeto disponível no repositório do
% GitHub mencionado acima. Para informações sobre como executar, contribuir ou
% obter resultados, consulte o README.md no repositório.
%
% Última atualização: dezembro de 2023
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(apply)).
:- use_module(library(csv)).

:- if(current_predicate(swish_render:use_rendering/2)).
:- use_rendering(table,
                 [header(atribuição_bolsa('Bolsa', 'Candidato', 'Nível', 'Cota racial',
                                          'Agência', 'Modalidade', 'Disponibilidade'))]).
:- endif.

cotas_raciais(Nivel, L) :-
    findall(X, bolsa(X, Nivel, _, _, true, _), L1),
    ordena_bolsas(L1, L).

cotas_geral(Nivel, ExcedenteCotasRaciais, L) :-
    findall(X, bolsa(X, Nivel, _, _, false, _), L1),
    append(ExcedenteCotasRaciais, L1, L2),
    ordena_bolsas(L2, L).

cumpre_requisitos(capes, 1, C) :-
    dedicação_integral(C), sem_pendências(C).

cumpre_requisitos(capes, 2, C) :-
    sem_pendências(C).

%% Candidatos esgotados
distribui_bolsas(_, _, [], LB, EC, EC, EB1, EB, L, L) :-
    append(EB1, LB, EB),!. % Preserva restante das bolsas não atribuídas

%% Bolsas esgotadas
distribui_bolsas(_, _, LC, [], EC1, EC, EB, EB, L, L) :-
    append(EC1, LC, EC),!. % Preserva excedente de candidatos sem atribuição.

distribui_bolsas(Agencia, Tipo, [C|RestoCandidatos], [B|RestoBolsas],
           EC1, ExcedenteCandidatos,
           EB1, ExcedenteBolsas,
           L1, L) :-
    cumpre_requisitos(Agencia, Tipo, C),
    append(L1, [B/C], L2),
    distribui_bolsas(Agencia, Tipo, RestoCandidatos, RestoBolsas,
               EC1, ExcedenteCandidatos,
               EB1, ExcedenteBolsas,
               L2, L).

distribui_bolsas(Agencia, Tipo, [C|RestoCandidatos], LB,
           EC1, ExcedenteCandidatos,
           EB1, ExcedenteBolsas, L1, L) :-
    append(EC1, [C], EC2), % Candidato não cumpre requisito da bolsa, LB permanece
    distribui_bolsas(Agencia, Tipo, RestoCandidatos, LB, EC2, ExcedenteCandidatos,
               EB1, ExcedenteBolsas, L1, L).

distribui_bolsas(Agencia, Tipo, ListaCandidatos, ListaBolsas,
                 ExcedenteCandidatos, ExcedenteBolsas, L) :-
    distribui_bolsas(Agencia, Tipo, ListaCandidatos, ListaBolsas,
                    [], ExcedenteCandidatos, [], ExcedenteBolsas,
                    [], L).

tipo_bolsa(Nivel, Agencia, Modalidade, X) :-
    bolsa(X, Nivel, Agencia, Modalidade, _, _).

distribui_pelas_modalidades(Nivel, ListaCandidatos, ListaBolsas, EC, EB, L) :-
    include(tipo_bolsa(Nivel, capes, 1), ListaBolsas, LB1),
    distribui_bolsas(capes, 1, ListaCandidatos, LB1, EC1, EB1, L1),
    include(tipo_bolsa(Nivel, capes, 2), ListaBolsas, LB2),
    distribui_bolsas(capes, 2, EC1, LB2, EC, EB2, L2),
    append(EB1, EB2, EB),
    append(L1, L2, L).

distribui_cotas_raciais(Nivel, ListaCandidatos, EC, EB, L) :-
    cotas_raciais(Nivel, CR),
    distribui_pelas_modalidades(Nivel, ListaCandidatos, CR, EC, EB, L).

distribui_tudo(Nivel, ListaCandidatos, EC, EB, DistRacial, SelecaoGeral) :-
    %% distribui cotas raciais primeiro
    include(racial_declarado, ListaCandidatos, ListaCotistas),
    distribui_cotas_raciais(Nivel, ListaCotistas, _, EB1, DistRacial),

    %% na lista geral são removidos os candidatos já atribuídos para cota racial,
    %% sem modificação da ordem
    findall(C, member(_/C, DistRacial), AtribuidosRacial),
    subtract(ListaCandidatos, AtribuidosRacial, ListaCandidatosGeral),

    %% distribuição das cotas após critério racial
    cotas_geral(Nivel, EB1, LBGeral),
    distribui_pelas_modalidades(Nivel, ListaCandidatosGeral, LBGeral, EC, EB, SelecaoGeral),
    !.

resultado_completo(L) :-
    candidatos(doutorado, LC1),
    distribui_tudo(doutorado, LC1, _EC1, _EB1, SR1, SG1),
    candidatos(mestrado, LC2),
    distribui_tudo(mestrado, LC2, _EC2, _EB2, SR2, SG2),
    append(SR1, SG1, L1),
    append(SR2, SG2, L2),
    append(L1, L2, L).


atribui_bolsa(Bolsa/Candidato, Atribuicao) :-
    bolsa(Bolsa, Nivel, Agencia, Modalidade, Racial, Disponibilidade),
    Atribuicao =.. [atribuição_bolsa, Bolsa, Candidato, Nivel, Racial, Agencia, Modalidade, Disponibilidade].

atribuições_bolsas(A) :-
    resultado_completo(L),
    maplist(atribui_bolsa, L, A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados auxiliares %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Define a ordem dos meses
ordem_mes(jan, 1). ordem_mes(fev, 2). ordem_mes(mar, 3). ordem_mes(abr, 4).
ordem_mes(mai, 5). ordem_mes(jun, 6). ordem_mes(jul, 7). ordem_mes(ago, 8).
ordem_mes(set, 9). ordem_mes(out, 10). ordem_mes(nov, 11). ordem_mes(dez, 12).

data_disponibilidade(Bolsa, Ano-MesNum) :-
    bolsa(Bolsa,_,_,_,_,Data),
    atomic_list_concat([Mes, AnoStr], '/', Data),
    atom_number(AnoStr, Ano),
    ordem_mes(Mes, MesNum).

compara_data_disponibilidade(Delta, B1, B2) :-
    data_disponibilidade(B1, Ano1-Mes1),
    data_disponibilidade(B2, Ano2-Mes2),
    compare(Delta, Ano1-Mes1-B1, Ano2-Mes2-B2). % B1/B2 aqui evitam duplicidade

ordena_bolsas(ListaBolsas, ListaBolsasOrdenada) :-
    predsort(compara_data_disponibilidade, ListaBolsas, ListaBolsasOrdenada).


%%%%%%%%%%%%%%
%% DATABASE %%
%%%%%%%%%%%%%%

:- dynamic candidato/5, bolsa/6.

carrega_candidatos :-
    retractall(candidato(_,_,_,_,_)),
    csv_read_file("../data/candidatos_bolsas.csv", [_Header|Rows],
                  [functor(candidato), arity(5), separator(0',)]),
    maplist(assert, Rows).

carrega_bolsas :-
    retractall(bolsa(_,_,_,_,_,_)),
    csv_read_file("../data/bolsas_disponíveis.csv", [_Header|Rows],
                  [functor(bolsa), arity(6), separator(0',)]),
    maplist(assert, Rows).

carrega_dados_locais :-
    carrega_bolsas,
    carrega_candidatos.

grava_distribuição :-
    atribuições_bolsas(A),
    append([atribuição_bolsa('Bolsa', 'Candidato', 'Nível', 'Cota racial',
            'Agência', 'Modalidade', 'Disponibilidade')], A, Rows),
    csv_write_file("../output/distribuição_bolsas.csv", Rows).

candidato(X, Nivel) :-
    candidato(X, Nivel, _, _, _).

candidatos(Nivel, L) :-
    findall(X, candidato(X, Nivel), L).

racial_declarado(X) :-
    candidato(X, _, true, _, _).

dedicação_integral(X) :-
    candidato(X, _, _, true, _).

sem_pendências(X) :-
    candidato(X, _, _, _, false).

%% Exemplos:
%% ?- candidatos(mestrado, L), distribui_tudo(mestrado, L, EC, EB, DistRacial, Geral).
%% L = ['Gabriel Lemes Duarte', 'Antonio Carlos Figueiredo Ferraz Ferreira', 'Pedro Navarro Artoni', 'ALESSANDRA ALVES PELEGRINI', 'Vinícius Ruiz Barbanti', 'Lucas Rodrigues Bello', 'Raul Signorini Quintão', 'João Martins Timóteo da Costa', 'José Kapundi Rodrigues'],
%% EC = ['Lucas Rodrigues Bello', 'Raul Signorini Quintão', 'João Martins Timóteo da Costa', 'José Kapundi Rodrigues'],
%% EB = [],
%% DistRacial = [6/'Gabriel Lemes Duarte'],
%% Geral = [7/'Antonio Carlos Figueiredo Ferraz Ferreira', 8/'Pedro Navarro Artoni', 9/'ALESSANDRA ALVES PELEGRINI', 10/'Vinícius Ruiz Barbanti'].
%%
%% ?- carrega_dados, atribuições_bolsas(A).
%% A = [atribuição_bolsa(1, 'Antonio José Barreto Filho', doutorado, true, capes, 1, 'jan/24'), atribuição_bolsa(2, 'Sérgio Alexandre Minehira', doutorado, true, capes, 2, 'jan/24'), atribuição_bolsa(3, 'Renan Evangelista Silva', doutorado, false, capes, 2, 'fev/24'), atribuição_bolsa(4, 'André Christian Dalpicolo', doutorado, false, capes, 2, 'fev/24'), atribuição_bolsa(5, 'Abigail Campos Leal', doutorado, false, capes, 2, 'mar/24'), atribuição_bolsa(6, 'Gabriel Lemes Duarte', mestrado, true, capes, 1, 'jan/24'), atribuição_bolsa(7, 'Antonio Carlos Figueiredo Ferraz Ferreira', mestrado, false, capes, 1, 'fev/24'), atribuição_bolsa(8, 'Pedro Navarro Artoni', mestrado, false, capes, 2, 'mar/24'), atribuição_bolsa(..., ..., ..., ..., ..., ..., ...)|...].
