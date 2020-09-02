% nal_reader.pl
% Read Non-Axiomatic Logic from Prolog
:-module(nal_reader,[
            test_nal/0,
            test_nal/1, 
            test_nal/2,
            call_nal/2,
            call_nal/3                   
         ]).

:- set_module(class(library)).
:- set_module(base(system)).

:- use_module(library(logicmoo_common)).

a_nal_test("'Revision ------

'Bird is a type of swimmer.
<bird --> swimmer>.

'Bird is probably not a type of swimmer.
<bird --> swimmer>. %0.10;0.60%

1

'Bird is very likely to be a type of swimmer.
''outputMustContain('<bird --> swimmer>. %0.87;0.91%')").

a_nal_test("'the detective claims that tim lives in graz
'<{tim} --> (/,livingIn,_,{graz})>.
'and lawyer claims that this is not the case
<{tim} --> (/,livingIn,_,{graz})>. %0%
100
'the first deponent, a psychologist,
'claims that people with sunglasses are more aggressive
<<(*,$1,sunglasses) --> own> ==> <$1 --> [aggressive]>>.
'the third deponent claims, that he has seen tom with sunglasses on:
<(*,{tom},sunglasses) --> own>.
'the teacher claims, that people who are aggressive tend to be murders
<<$1 --> [aggressive]> ==> <$1 --> murder>>.
'the second deponent claims, that if the person lives in Graz, he is surely the murder
<<$1 --> (/,livingIn,_,{graz})> ==> <$1 --> murder>>.
'who is the murder? 
<{?who} --> murder>?
''outputMustContain('<{tom} --> murder>. %1.00;0.73%')").



do_reader_tests:- forall(a_nal_test(Test),try_reader_test(Test)).



try_reader_test(Test):- call_nal('dmsg',Test,Out).


zave_varname(N,V):- debug_var(N,V),!.
%zave_varname(N,V):- V = '$VAR'(N).

implode_varnames(Vs):- (var(Vs) ; Vs==[]),!.
implode_varnames([NV|Vs]) :- implode_varnames(Vs),
  (var(NV) -> ignore((variable_name(NV,Name),zave_varname(Name,NV))); 
  ignore((NV=(N=V),zave_varname(N,V)))).


read_nal_clause( NonStream, Out):- \+ is_stream(NonStream), !, % wdmsg(NonStream),
  must_or_rtrace((open_string(NonStream,Stream), read_nal_clause(Stream, Out))).

read_nal_clause(Stream, Out):-
 '$current_typein_module'(M),
  M\== input, !,
  setup_call_cleanup(
   '$set_typein_module'(input),
   read_nal_clause(Stream, Out),
   '$set_typein_module'(M)).

read_nal_clause(Stream, Out):-
 op(601, xfx, input:(/)),
 op(601, xfx, input:(\\)),
 (at_end_of_stream(Stream)-> Out=[]; 
   (read_nal_term(Stream, Term),
    (Term == end_of_file -> Out=[];
      (Term = (:- Exec) -> (input:call(Exec), Out=More) ; Out = [Term|More]),
       read_nal_clause(Stream, More)))),!.

% Expand Stream or String
call_nal(Ctx, Stream, Out):- \+ compound(Stream),
  must_or_rtrace(read_nal_clause(Stream, List)), !,
  call_nal(Ctx, List, Out).

call_nal(Ctx, List, Out):- is_list(List),!, maplist(call_nal(Ctx),List, Out).
call_nal(Ctx, InnerCtx=json(List), Out):- !,  call_nal([InnerCtx|Ctx], List, Out).

call_nal(Ctx, List, Out):- 
   sub_term(Sub, List), nonvar(Sub), 
   rule_rewrite(Ctx, Sub, NewSub),
   % ignore((NewSub=='$',wdmsg(rule_rewrite(_Ctx, Sub, NewSub)))),
   nonvar(NewSub), Sub\==NewSub,
   subst(List, Sub, NewSub, NewList), 
   List\==NewList, !, 
   call_nal(Ctx, NewList, Out).

call_nal(_Ctx, List, Out):- flatten([List], Out),!.




rule_rewrite(_Ctx, json(Replace), Replace):- nonvar(Replace),!.


join_atomics(Sep,List,Joined):- atomics_to_string(List,Sep,Joined).

into_nal_tokenized(Text,TokenizedText):- \+ string(Text),!, 
  any_to_string(Text,String), into_nal_tokenized(String,TokenizedText).
into_nal_tokenized(Text,TokenizedText):-
 split_string(Text, "", "\s\t\r\n", [L]), L\==Text,!,
 into_nal_tokenized(L,M), 
 %string_concat(M,"\n",TokenizedText).
 string_concat(M,"",TokenizedText).
into_nal_tokenized(Text,TokenizedText):-   L=[_S1,_S2|_SS],    
  member(Split,["\n'","'\n","<META>'","<META>","\n"]),  
  atomic_list_concat(L,Split,Text),  
  maplist(into_nal_tokenized,L,LO),
  atomics_to_string(LO,Split, TokenizedText).
into_nal_tokenized(Text,TokenizedText):-   
  split_string(Text, "\n", "\s\t\n\r",StringList),
  maplist(into_text80_atoms,StringList,SentenceList),
  maplist(join_atomics(' '),SentenceList,ListOfStrings),
  join_atomics('\n',ListOfStrings,TokenizedText),!.





  read_nal(Stream,Term,Vs),wdmsg(Term-Vs),!.

try_reader_test(Test):- is_stream(Test), !, \+ is_compound(Test), open_string(Test,Stream), try_reader_test(Stream).
  read_nal(Stream,Term,Vs),wdmsg(Term-Vs),!.

