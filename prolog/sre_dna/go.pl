
% ?- load_files(library(random)).
% ?- load_files(library(date)).
% ?- load_files(library(strings)).

?- use_module(library(random)).
?- use_module(library(system)).
?- use_module(library(lists)).

?- consult('dynamics').
?- consult('dctg').
?- consult('parameters_P').
?- consult('operators').
?- consult('dctg_pp').
?- consult('utils').
?- dctg_file_P(FileDCTG),grammar(FileDCTG), make_grammar_table.
?- tell(compile_file),     % fast: new
	write('?- use_module(library(lists)).'),nl,
	listing,
	told.

?- consult('ccs_utils').
?- consult('dctg_gen').
?- consult('dctg_reprod').
?- consult('dctg_utils').
?- consult('generate').
?- consult('gp_engine').
?- consult('lamarckian').
?- consult('evaluation').
?- consult('file_stats').

% following must follow 'parameters_P' above.
% Convenient to put here, as interactive debugging of DCTG-GP is easier.

?- consult('sre_mutation3').
?- consult('sre_crossover3a').
?- consult('dna_proc').
?- consult('mask_optimizer').

?- dna_file_P(DNA_file), consult(DNA_file).

?- fitness_func_P(File), consult(File). 

?- consult(fast:compile_file).
?- clean_up.

