% nal_reader.pl
% Read Non_Axiomatic Logic from Prolog
:-module(nal_reader,[
            test_nal/0,
            test_nal/1, 
           % test_nal/2,
          %  call_nal/2,
            call_nal/3                   
         ]).

:- set_module(class(library)).
:- set_module(base(system)).

:- use_module(library(logicmoo_common)).

/*
                task ::= [budget] sentence                       (* task to be processed *)

         sentence ::= statement"." [tense] [truth]            (* judgement to be absorbed into beliefs *)
                    | statement"?" [tense] [truth]            (* question on thuth-value to be answered *)
                    | statement"!" [desire]                   (* goal to be realized by operations *)
                    | statement"@" [desire]                   (* question on desire-value to be answered *)

        statement ::= <"<">term copula term<">">              (* two terms related to each other *)
                    | <"(">term copula term<")">              (* two terms related to each other, new notation *)
                    | term                                    (* a term can name a statement *)
                    | "(^"word {","term} ")"                  (* an operation to be executed *)
                    | word"("term {","term} ")"               (* an operation to be executed, new notation *)

           copula ::= "-->"                                   (* inheritance *)
                    | "<->"                                   (* similarity *)
                    | "{--"                                   (* instance *)
                    | "--]"                                   (* property *)
                    | "{-]"                                   (* instance-property *)
                    | "==>"                                   (* implication *)
                    | "=/>"                                   (* predictive implication *)
                    | "=|>"                                   (* concurrent implication *)
                    | "=\\>"                                  (* =\> retrospective implication *)
                    | "<=>"                                   (* equivalence *)
                    | "</>"                                   (* predictive equivalence *)
                    | "<|>"                                   (* concurrent equivalence *)

             term ::= word                                    (* an atomic constant term *)
                    | variable                                (* an atomic variable term *)
                    | compound-term                           (* a term with internal structure *)
                    | statement                               (* a statement can serve as a term *)

    compound-term ::= op-ext-set term {"," term} "}"          (* extensional set *)
                    | op-int-set term {"," term} "]"          (* intensional set *)
                    | "("op-multi"," term {"," term} ")"      (* with prefix operator *)
                    | "("op-single"," term "," term ")"       (* with prefix operator *)
                    | "(" term {op-multi term} ")"            (* with infix operator *)
                    | "(" term op-single term ")"             (* with infix operator *)
                    | "(" term {","term} ")"                  (* product, new notation *)
                    | "(" op-ext-image "," term {"," term} ")"(* special case, extensional image *)
                    | "(" op-int-image "," term {"," term} ")"(* special case, \ intensional image *)
                    | "(" op-negation "," term ")"            (* negation *)
                    | op-negation term                        (* negation, new notation *)

        op-int-set::= "["                                     (* intensional set *)
        op-ext-set::= "{"                                     (* extensional set *)
       op-negation::= "--"                                    (* negation *)
      op-int-image::= "\\"                                    (* \ intensional image *)
      op-ext-image::= "/"                                     (* extensional image *)
         op-multi ::= "&&"                                    (* conjunction *)
                    | "*"                                     (* product *)
                    | "||"                                    (* disjunction *)
                    | "&|"                                    (* parallel events *)
                    | "&/"                                    (* sequential events *)
                    | "|"                                     (* intensional intersection *)
                    | "&"                                     (* extensional intersection *)
        op-single ::= "-"                                     (* extensional difference *)
                    | "~"                                     (* intensional difference *)

         variable ::= "$"word                                 (* independent variable *)
                    | "#"word                                 (* dependent variable *)
                    | "?"word                                 (* query variable in question *)

            tense ::= ":/:"                                   (* future event *)
                    | ":|:"                                   (* present event *)
                    | ":\\:"                                  (* :\: past event *)
          
           desire ::= truth                                   (* same format, different interpretations *)
            truth ::= <"%">frequency[<";">confidence]<"%">    (* two numbers in [0,1]x(0,1) *)
           budget ::= <"$">priority[<";">durability][<";">quality]<"$"> (* three numbers in [0,1]x(0,1)x[0,1] *)

               word : #"[^\ ]+"                               (* unicode string *)    
           priority : #"([0]?\.[0-9]+|1\.[0]*|1|0)"           (* 0 <= x <= 1 *)
         durability : #"[0]?\.[0]*[1-9]{1}[0-9]*"             (* 0 <  x <  1 *)
            quality : #"([0]?\.[0-9]+|1\.[0]*|1|0)"           (* 0 <= x <= 1 *)
          frequency : #"([0]?\.[0-9]+|1\.[0]*|1|0)"           (* 0 <= x <= 1 *)
         confidence : #"[0]?\.[0]*[1-9]{1}[0-9]*"             (* 0 <  x <  1 *)
*/

task(S)--> cwhite,!,task(S).
task(task(X,S,T,O,B)) --> task(X,S,T,O,B),!.



task(X,S,T,O,B) --> optional(B, budget),!, sentence(X,S,T,O).  % task to be processed 


sentence(X,S,T,O)--> 
           statement(S), o(`.` ,X, judgement),!, optional(T,tense), optional(O,truth)   % judgement to be absorbed into beliefs 
        ;  statement(S), o(`?` ,X, question_truth),!, optional(T,tense), optional(O,truth)      % question on truth_value to be answered 
        ;  statement(S), o(`!` ,X, goal), optional(O,desire)                  % goal to be realized by operations 
        ;  statement(S), o(`@` ,X, question_desire), optional(O,desire)       % question on desire_value to be answered 
        .

statement(S)--> maybe_some_white(statement0(S)).
statement0(S)--> 
        `<` ,!, term(A), copula(R), term(B), `>` ,   {S=..[R,A,B]}            % two, terms related to each other 
      ;  l_paren, `^` , term_list(L), paren_r,       {S= exec(L)}             % an operation to be executed 
      ;  l_paren, term(A), copula(R), term(B), `)`,  {S=..[R,A,B]}            % two, terms related to each other, new notation 
      ;  word(A), l_paren, term_list(L), paren_r,    {S= exec([A|L])}         % an operation to be executed, new notation 
      ;  term1(X),                                   {S= statement(X)}        % a, term, can name a statement(S) 
      .
         

copula(X) -->      
            o(`-->` ,X,                                                  inherits )
         ;  o(`<->` ,X,                                                  similar )
         ;  o(`{--` ,X,                                                  instance )
         ;  o(`--]` ,X,                                                  property )
         ;  o(`{-]` ,X,                                                  instance_prop )
         ;  o(`==>` ,X,                                                  implication )
         ;  o(`=/>` ,X,                                                  predictive_impl )
         ;  o(`=|>` ,X,                                                  concurrent_impl )
         ;  o(`=\\>` ,X,                                                 retrospective_impl )
         ;  o(`<=>` ,X,                                                  equiv )
         ;  o(`</>` ,X,                                                  predictive_equiv )
         ;  o(`<|>` ,X,                                                  concurrent_equiv )
         ;  o(`=>` ,X,                                                  unknown_impl )
         .

term(S)--> word(S)                                                              % an atomic constant, term,                 
        ;  variable(S)                                                          % an atomic variable, term, 
        ;  compound_term(S)                                                     % a, term, with internal structure 
        ;  statement(S)                                                         % a statement can serve as a, term, 
        .

term1(S)--> word(S)                                                             % an atomic constant, term,                 
        ;  variable(S)                                                          % an atomic variable, term, 
        ;  compound_term(S)                                                     % a, term, with internal structure 
        .

compound_term(S)--> nal_dcgUnless(`<`),!,
(
      o(op_ext_set,X,ext_set), term_list(L), `}`                                                     % extensional set 
   ;  o(op_int_set,X,int_set), term_list(L), `]`                                                     % intensional set 
   ;  l_paren, op_multi(X), comma, term_list(L), paren_r                                     % with prefix operator 
   ;  l_paren, op_single(X), comma, term(A), comma, term(B), paren_r, {L=[A,B]}              % with prefix operator 
   ;  l_paren, o(op_ext_image,X), comma, term_list(L), paren_r                               % special case, extensional image 
   ;  l_paren, o(op_int_image,X), comma, term_list(L), paren_r                               % special case, \ intensional image 
   ;  l_paren, o(op_negation,X), comma, term(AB), paren_r,{S= neg(AB)}                       % negation 
   ;  op_negation, term(AB),{S= neg(AB)}                                                     % negation, new notation 
   ;  l_paren, term(A), op_multi(X), term(B), paren_r,{L=[A,B]}                              % with infix operator 
   ;  l_paren, term(A), op_single(X), term(B), paren_r,{L=[A,B]}                             % with infix operator 
   ;  l_paren, {X=product}, term_list(L), paren_r                                            % product, new notation 
   ), {var(S)-> S=..[X,L] ; true}.

op_int_set-->`[`.                                                                       % intensional set 
op_ext_set-->`{`.                                                                       % extensional set 
op_negation-->`--`.                                                                     % negation 
op_int_image-->`\\`.                                                                    % \ intensional image 
op_ext_image-->`/`.                                                                     % / extensional image 

op_multi(X)-->   
                o(`&&` ,X, and)                                                               % conjunction 
              ; o(`*` ,X, product)                                                            % product 
              ; o(`||` ,X, or)                                                                % disjunction 
              ; o(`&|` ,X, parallel_evnts)                                                    % parallel events 
              ; o(`&/` ,X, sequence_evnts)                                                    % sequential events 
              ; o(`|` ,X, int_img)                                                            % intensional intersection 
              ; o(`&` ,X, ext_img)                                                            % extensional intersection 
              .
op_single(X) --> 
          o(`-`, X, ext_diff)                                                         % extensional difference 
       ;  o(`~`, X, int_diff)                                                         % intensional difference 
       .

variable(var(X,W))
    -->o(`$`, X, ind), word0(W)                                                % independent variable 
      ;o(`#`, X, dep), word0(W)                                                % dependent variable 
      ;o(`?`, X, query), word0(W)                                              % query variable in question 
      .

tense(X) -->
      o(`:/:`, X, future)                                                     % future event 
   ;  o(`:|:`, X, present)                                                    % present event 
   ;  o(`:\\:`, X, past)                                                      % :\: past event 
   .

% Desire is same format of Truth, but different interpretations 
desire(D)-->truth(D).									
% Truth is two numbers in [0,1]x(0,1) 
truth([F,C])--> `{`, !, frequency(F), confidence(C), `}`.	                
truth([F,C])--> `%`, frequency(F), optional((`;`, confidence(C))), `%`.	                
% Budget is three numbers in optional(O,0,1]x(0,1)x[0,1] 
budget(budget_pdq(P,D,Q))--> `$`,!, priority(P), optional(( `;`, durability(D))), optional((`;`, quality(Q))), `$`.  


word(E) --> maybe_some_white(word0(E)).

word0(E) --> dcg_basics:number(E),!.
word0(E) --> s_string(E),!.
word0(E) --> nal_peek([C]),{char_type(C,alpha)},!, rsymbol([],E),!.

s_string(Text)                 --> `"`, !, zalwayz(s_string_cont(Text)),!.
s_string_cont("")             --> `"`,!.
s_string_cont(Txt)                 --> dbl_quoted_string(S), {text_to_string_safe(S,Txt)}.
dbl_quoted_string(X)--> read_string_until(X,`"`).

  priority(F) --> float_inclusive(0,1,F).           %  0 <= x <= 1 
durability(F) --> float_exclusive(0,1,F).           %  0 <  x <  1 
   quality(F) --> float_inclusive(0,1,F).           %  0 <= x <= 1 
 frequency(F) --> float_inclusive(0,1,F).           %  0 <= x <= 1 
confidence(F) --> float_exclusive(0,1,F).           %  0 <  x <  1 

o(S,X,X) --> owhite,S,owhite.
o(X,X) --> o(X,X,X).

float_inclusive(L,H,F)--> maybe_some_white((dcg_basics:number(F) -> {warn_on_falure((L=< F,F=< H))})).
float_exclusive(L,H,F)--> maybe_some_white((dcg_basics:number(F) -> {warn_if_strict((L < F,F < H))})).

warn_if_strict(G):- call(G),!.
warn_if_strict(G):- dmsg(warn_if_strict(G)),!.

optional(X) --> cwhite, !, optional(X).
optional(X) --> X, owhite.
optional(_) --> [].
optional(O,X) --> {debug_var(X,O),append_term(X,O,XO)},!,optional(XO).

maybe_some_white(X) --> owhite,!,X,owhite.  
owhite --> cwhite.
owhite --> [].

% cwhite --> comment_expr(S,I,CP),!,{assert(t_l:s_reader_info('$COMMENT'(S,I,CP)))},!,owhite.
cwhite --> comment_expr(CMT),!,{assert(t_l:s_reader_info(CMT))},!,owhite.
cwhite --> [C], {nonvar(C),charvar(C),!,bx(C =< 32)},!,owhite.
charvar(C):- integer(C)-> true; (writeln(charvar(C)),break,fail).
comment_expr('$COMMENT'(Expr,I,CP)) --> comment_expr_3(Expr,I,CP),!.

comment_expr_3(T,N,CharPOS) --> `/*`, !, my_lazy_list_location(file(_,_,N,CharPOS)),!, zalwayz(read_string_until_no_esc(S,`*/`)),!,
  {text_to_string_safe(S,T)},!.
comment_expr_3(T,N,CharPOS) -->  `//`,!, my_lazy_list_location(file(_,_,N,CharPOS)),!,zalwayz(read_string_until_no_esc(S,eoln)),!,
  {text_to_string_safe(S,T)},!.
comment_expr_3(T,N,CharPOS) -->  `'`,!, my_lazy_list_location(file(_,_,N,CharPOS)),!,zalwayz(read_string_until_no_esc(S,eoln)),!,
  {text_to_string_safe(S,T)},!.

eoln --> [C],!, {charvar(C),eoln(C)},!.
eoln(10).
eoln(13).

comma --> maybe_some_white(`,`).
l_paren --> maybe_some_white(`(`).
paren_r --> maybe_some_white(`)`).

term_list([H|T]) --> term(H), ( comma ->  term_list(T) ; {T=[]} ).


parse_nal_term(S, Expr) :- is_stream(S),!, parse_nal_stream(S,Expr).
parse_nal_term(string(String), Expr) :- !,parse_nal_ascii(String, Expr).
parse_nal_term(atom(String), Expr) :- !,parse_nal_ascii(String, Expr).
parse_nal_term(text(String), Expr) :- !,parse_nal_ascii(String, Expr).
parse_nal_term((String), Expr) :- string(String),!,parse_nal_ascii(String, Expr).
parse_nal_term([E|List], Expr) :- !, parse_nal_ascii([E|List], Expr).
parse_nal_term(Other, Expr) :- quietly((l_open_input(Other,In)->Other\=@=In)),!,parse_nal_term(In, Expr).

rsymbol(Chars,E) --> [C], {sym_char(C)},!, sym_continue(S), {append(Chars,[C|S],AChars),string_to_atom(AChars,E)},!.
sym_continue([H|T]) --> [H], {sym_char(H)},!, sym_continue(T).
sym_continue([]) --> peek_symbol_breaker,!.
sym_continue([]) --> [].
nal_peek(Grammar,List,List):- phrase(Grammar,List,_),!.
nal_dcgUnless(Grammar,List,List):- \+ phrase(Grammar,List,_),!.
peek_symbol_breaker --> nal_peek([C]),{\+ sym_char(C)}.
peek_symbol_breaker --> one_blank.
one_blank --> [C],!,{C =< 32}.   
sym_char(C):- bx(C =<  32),!,fail.
%sym_char(44). % allow comma in middle of symbol
% word is: #"[^\ ]+"   %  unicode string     
sym_char(C):- memberchk(C,`";()~'[]<>{},=-\\^```),!,fail.  % maybe 44 ? comma
%sym_char(C):- nb_current('$maybe_string',t),memberchk(C,`,.:;!%`),!,fail.
sym_char(_):- !.

rsymbol_cont(Prepend,E) --> sym_continue(S), {append(Prepend,S,AChars),string_to_atom(AChars,E)},!.
my_lazy_list_location(Loc) --> lazy_list_location(Loc),!.
my_lazy_list_location(file(_,_,-1,-1))-->[].
read_string_until_no_esc(String,End)--> read_string_until(noesc,String,End).
read_string_until(String,End)--> read_string_until(esc,String,End).
read_string_until(_,[],eoln,S,E):- S==[],!,E=[].
read_string_until(esc,[C|S],End) --> `\\`,!, zalwayz(escaped_char(C)),!, read_string_until(esc,S,End),!.
read_string_until(_,[],HB) --> HB, !.
read_string_until(Esc,[C|S],HB) --> [C],!,read_string_until(Esc,S,HB),!.
zalwayz(G,H,T):- phrase(G,H,T),!.
zalwayz(G,H,T):- nb_current('$nal_translation_stream',S),is_stream(S), \+ stream_property(S,tty(true)),always_b(G,H,T).
always_b(G,H,T):- break,H=[_|_],writeq(phrase_h(G,H,T)),dcg_print_start_of(H),writeq(phrase(G,H,T)),!,trace,ignore(rtrace(phrase(G,H,T))),!,notrace,dcg_print_start_of(H),writeq(phrase(G,H,T)), break,!,fail.
always_b(G,H,T):- writeq(phrase(G,H,T)),dcg_print_start_of(H),writeq(phrase(G,H,T)),!,trace,ignore(rtrace(phrase(G,H,T))),!,notrace,dcg_print_start_of(H),writeq(phrase(G,H,T)), break,!,fail.
dcg_print_start_of(H):- (length(L,3000);length(L,300);length(L,30);length(L,10);length(L,1);length(L,0)),append(L,_,H),!,format('~NTEXT: ~s~n',[L]),!.
bx(CT2):- notrace_catch_fail(CT2,E,(writeq(E:CT2),break)),!.
notrace_catch_fail(G,E,C):- notrace(catch(G,E,C)),!.
notrace_catch_fail(G):- notrace(catch(G,_,fail)),!.
zalwayz(G):- must(G).
clean_fromt_ws([],[]).
clean_fromt_ws([D|DCodes],Codes):- 
  ((\+ char_type(D,white), \+ char_type(D,end_of_line)) -> [D|DCodes]=Codes ; clean_fromt_ws(DCodes,Codes)).

:- export(txt_to_codes/2).
txt_to_codes(S,Codes):- notrace(is_stream(S)),!,stream_to_lazy_list(S,Codes),!.
txt_to_codes(AttVar,AttVarO):- notrace(attvar(AttVar)),!,AttVarO=AttVar.
% txt_to_codes([C|Text],[C|Text]):- integer(C),is_list(Text),!.
% txt_to_codes([C|Text],_):- atom(C),atom_length(C,1),!,throw(txt_to_codes([C|Text])).
txt_to_codes(Text,Codes):- notrace_catch_fail((text_to_string_safe(Text,String),!,string_codes(String,Codes))),!.

phrase_from_pending_stream(Grammar, In):-
   remove_pending_buffer_codes(In,CodesPrev),
   phrase_from_pending_stream(CodesPrev, Grammar, In).

phrase_from_pending_stream(CodesPrev,Grammar,In):- CodesPrev=[_,_|_],
   phrase(Grammar,CodesPrev,NewBuffer),!,
   append_buffer_codes(In,NewBuffer).
phrase_from_pending_stream(CodesPrev,Grammar,In):- 
  b_setval('$nal_translation_stream',In),
  read_codes_from_pending_input(In,Codes),!,
  ((is_eof_codes(Codes)) -> 
     phrase_from_eof(Grammar, In); 
     (append(CodesPrev,Codes,NewCodes), !,
       (phrase(Grammar, NewCodes, NewBuffer) 
        -> append_buffer_codes(In,NewBuffer);
          phrase_from_pending_stream(NewCodes,Grammar,In)))).


:- thread_local(t_l:fake_buffer_codes/2).

%% parse_nal_stream( +Stream, -Expr) is det.
%
% Parse S-expression from a Stream
%
parse_nal_stream(S,Expr):- 
  catch(
    parse_nal_stream_1(S,Expr),
    end_of_stream_signal(_Gram,S),
    Expr=end_of_file).
parse_nal_stream_1(S,Expr):-
  phrase_from_stream_nd(file_nal_with_comments(Expr),S).

%phrase_from_stream_nd(Grammar, In) :-  at_end_of_stream(In),trace,!,phrase_from_eof(Grammar, In).

is_tty_alive(In):-
  stream_property(In,tty(true)),
  stream_property(In,mode(read)),
  stream_property(In,end_of_stream(not)).

show_stream_info(In):-
     notrace(( forall(stream_property(In,(BUF)),
    (writeq(show_stream_info(In,(BUF))),nl)))),!.

phrase_from_stream_nd(Grammar,In):- 
   notrace((peek_pending_codes(In,Codes)->Codes=[_,_|_],
   remove_pending_buffer_codes(In,_))),
   (phrase(Grammar,Codes,NewBuffer)-> append_buffer_codes(In,NewBuffer);( append_buffer_codes(In,Codes),fail)).
                                                       
phrase_from_stream_nd(Grammar, In) :- at_end_of_stream(In), peek_pending_codes(In,Pend),is_eof_codes(Pend),!,phrase_from_eof(Grammar, In). %
%phrase_from_stream_nd(Grammar, _) :- clause(t_l:s_reader_info(I),_,Ref),I=Grammar,erase(Ref).
phrase_from_stream_nd(Grammar, In) :- stream_property(In,tty(true)),!,repeat,is_tty_alive(In),phrase_from_pending_stream(Grammar, In).
phrase_from_stream_nd(Grammar, In) :- stream_property(In,file_name(_Name)),!,
    if_debugging(sreader,show_stream_info(In)),
    read_stream_to_codes(In,Codes),
    b_setval('$lisp_translation_stream',In),
    append_buffer_codes(In,Codes),!,
    phrase_from_buffer_codes(Grammar,In).

phrase_from_stream_nd(Grammar, In) :- \+ supports_seek(In),!,
    if_debugging(sreader,show_stream_info(In)),
    read_stream_to_codes(In,Codes),
    b_setval('$lisp_translation_stream',In),
    append_buffer_codes(In,Codes),!,
    phrase_from_buffer_codes(Grammar,In).

phrase_from_stream_nd(Grammar, In) :- \+ supports_seek(In),!, phrase_from_pending_stream(Grammar, In).
%phrase_from_stream_nd(Grammar, In) :- b_setval('$lisp_translation_stream',In), quietly(phrase_from_stream_nd(Grammar, In)).
phrase_from_stream_nd(Grammar, In) :-  supports_seek(In),
    %set_stream(In,buffer_size(819200)),set_stream(In,buffer_size(16384)), set_stream(In,encoding(octet)), set_stream(In,timeout(3.0)),    
    %set_stream(In,buffer_size(5)), set_stream(In,encoding(octet)), set_stream(In,timeout(3.0)),set_stream(In,type(text)),%set_stream(In,buffer(false)),    
    character_count(In, FailToPosition),
    ((phrase_from_stream_lazy_part(Grammar, In) -> true ; (seek(In,FailToPosition,bof,_),!,fail))),!.


phrase_from_stream_lazy_part(Grammar, In):- 
    check_pending_buffer_codes(In),
    seek(In, 0, current, Prev),
    stream_to_lazy_list(In, List),
    nb_setval('$lisp_translation_line',Prev),!,
    phrase(Grammar, List, More) ->
    zalwayz((
       length(List,Used),!,
       length(More,UnUsed),!,
       if_debugging(sreader,wdmsg((Offset is Used - UnUsed + Prev))),
       bx(zalwayz(Offset is Used - UnUsed + Prev)),
       % dbginfo((Offset is Used - UnUsed + Prev)) ->
       seek(In,Offset,bof,_NewPos))).
%phrase_from_stream_lazy_part(Grammar, In):- phrase_from_file_part_c(Grammar, In).



peek_pending_codes(In,Codes):- (t_l:fake_buffer_codes(In,DCodes);Codes=[]),!,clean_fromt_ws(DCodes,Codes).

check_pending_buffer_codes(In):- peek_pending_codes(In,Codes),
  (Codes==[]->true;(throw(remove_pending_buffer_codes(In,Codes)))),!.

clear_pending_buffer_codes:- forall(retract(t_l:fake_buffer_codes(_In,_DCodes)),true).
remove_pending_buffer_codes(In,Codes):- retract(t_l:fake_buffer_codes(In,DCodes)),!,clean_fromt_ws(DCodes,Codes).
remove_pending_buffer_codes(_In,[]). % for first read

append_buffer_codes(In,Codes):- retract(t_l:fake_buffer_codes(In,CodesPrev)),!,append(CodesPrev,Codes,NewCodes),assertz(t_l:fake_buffer_codes(In,NewCodes)),!.
append_buffer_codes(In,Codes):- assertz(t_l:fake_buffer_codes(In,Codes)),!.

wait_on_input(In):- stream_property(In,end_of_stream(Not)),Not\==not,!.
wait_on_input(In):- repeat,wait_for_input([In],List,1.0),List==[In],!.

read_codes_from_pending_input(In,Codes):- \+ is_stream(In),!,remove_pending_buffer_codes(In,Codes).
read_codes_from_pending_input(In,Codes):- stream_property(In,end_of_stream(Not)),Not\==not,!,(Not==at->Codes=end_of_file;Codes=[-1]).
read_codes_from_pending_input(In,Codes):-  stream_property(In, buffer(none)),!,
   repeat,
    once(( wait_on_input(In),
    read_pending_codes(In,Codes,[]))),
    (Codes==[] -> (sleep(0.01),fail); true),!.
read_codes_from_pending_input(In,[Code|Codes]):-  get_code(In,Code),read_pending_codes(In,Codes,[]),!.
throw_reader_error(Error):- wdmsg(throw(reader_error(Error))),dumpST,wdmsg(throw(reader_error(Error))),throw(reader_error(Error)).

supports_seek(In):- notrace_catch_fail(stream_property(In,reposition(true))).
% supports_seek(In):- quietly_sreader(( notrace_catch_fail((notrace_catch_fail((seek(In, 1, current, _),seek(In, -1, current, _)),error(permission_error(reposition, stream, _), _Ctx),fail)),error(_,_),true))).

phrase_from_eof(Grammar, _):- var(Grammar),!,unify_next_or_eof(Grammar),!.
%phrase_from_eof(Grammar, _):- compound(Grammar),!,arg(1,Grammar,TV),unify_next_or_eof(TV),!.
phrase_from_eof(Grammar, _):- term_variables(Grammar,[TV|_]),unify_next_or_eof(TV),!.
phrase_from_eof(Grammar, In):- throw(end_of_stream_signal(Grammar,In)).

unify_next_or_eof(O) :- clause(t_l:s_reader_info(I),_,Ref),!,I=O,erase(Ref).
unify_next_or_eof(end_of_file).


%% parse_nal_ascii( +Codes, -Expr) is det.
%
% Parse S-expression Codes.
%
parse_nal_ascii(S, Expr) :- is_stream(S),!,parse_nal_stream(S,Expr).
%parse_nal_ascii(S, Expr) :- open_string(S,SIS),!,parse_nal_stream(SIS,Expr).
parse_nal_ascii(Text, Expr):- 
  notrace(txt_to_codes(Text,Codes)),
  =( ascii_,In),
  append_buffer_codes(In,Codes),!,
  phrase_from_buffer_codes_nd(file_nal_with_comments(Expr), In).

phrase_from_buffer_codes_nd(Grammar, In) :- peek_pending_codes(In,Pend),is_eof_codes(Pend),!,phrase_from_eof(Grammar,In).
phrase_from_buffer_codes_nd(Grammar, In) :- 
  repeat,
  ( phrase_from_buffer_codes(Grammar, In) *-> 
    ((peek_pending_codes(In,Pend),is_eof_codes(Pend))->!;true);(!,fail)).    

%phrase_from_buffer_codes(_Grammar, _In) :- peek_pending_codes(In,Pend),is_eof_codes(Pend),!,fail.
phrase_from_buffer_codes(Grammar, In):- 
   notrace((remove_pending_buffer_codes(In,NewCodes),
   NewCodes \== [])),!,
   (must_or_rtrace(phrase(Grammar, NewCodes, More))->append_buffer_codes(In,More);(append_buffer_codes(In,NewCodes),!,fail)).

is_eof_codes(Codes):- var(Codes),!,fail.
is_eof_codes(Codes):- Codes == [],!.
is_eof_codes(Codes):- Codes = [Code],!,is_eof_codes(Code).
is_eof_codes(end_of_file).
is_eof_codes(-1).

file_eof(I,O):- I==end_of_file,!,O=[].
file_eof --> [X],{ var(X), X = -1},!.
file_eof --> [X],{ attvar(X), X = -1},!.
file_eof --> [X],{ attvar(X), X = end_of_file},!.

%file_nal_with_comments(O) --> [], {clause(t_l:s_reader_info(O),_,Ref),erase(Ref)},!.
file_nal_with_comments(end_of_file) --> file_eof,!.
file_nal_with_comments(O) --> one_blank,!,file_nal_with_comments(O),!.  % WANT? 
file_nal_with_comments(C) --> comment_expr(C), owhite,!.
file_nal_with_comments(Out,S,E):- \+ t_l:sreader_options(with_text,true),!,phrase(file_nal(Out),S,E),!.
file_nal_with_comments(Out,S,E):- expr_with_text(Out,file_nal(O),O,S,E),!.

file_nal(end_of_file) --> file_eof,!.
% WANT? 
file_nal(O) --> cwhite,!,file_nal(O).
% file_nal(planStepLPG(Name,Expr,Value)) --> owhite,sym_or_num(Name),`:`,owhite, nal(Expr),owhite, `[`,sym_or_num(Value),`]`,owhite.  %   0.0003:   (PICK-UP ANDY IBM-R30 CS-LOUNGE) [0.1000]
% file_nal(Term,Left,Right):- eoln(EOL),append(LLeft,[46,EOL|Right],Left),read_term_from_codes(LLeft,Term,[double_quotes(string),syntax_errors(fail)]),!.
% file_nal(Term,Left,Right):- append(LLeft,[46|Right],Left), ( \+ member(46,Right)),read_term_from_codes(LLeft,Term,[double_quotes(string),syntax_errors(fail)]),!.
file_nal(do_steps(N)) --> dcg_basics:number(N),!.
file_nal(N=V) -->  maybe_some_white(`*`), word(N), maybe_some_white(`=`), term(V).
file_nal(H) -->  nal_peek([_]), task(H).
file_nal([])--> owhite,!.


expr_with_text(Out,DCG,O,S,E):- 
   zalwayz(lazy_list_character_count(StartPos,S,M)),%integer(StartPos),
   call(DCG,M,ME),
   lazy_list_character_count(EndPos,ME,E),!,
   expr_with_text2(Out,DCG,O,StartPos,M,ME,EndPos,S,E).

expr_with_text2(Out,_ ,O,StartPos,M,ME,EndPos,_,_):- 
   integer(StartPos),integer(EndPos),!,
   bx(Len is EndPos - StartPos),length(Grabber,Len),!,
   get_nal_with_comments(O,Grabber,Out,M,ME),!.
expr_with_text2(Out,_ ,O,end_of_file-StartPos,M,ME,end_of_file-EndPos,_,_):- 
   integer(StartPos),integer(EndPos),!,
   bx(Len is StartPos - EndPos),length(Grabber,Len),!,
   get_nal_with_comments(O,Grabber,Out,M,ME),!.

expr_with_text2(Out,DCG,O,StartPos,M,ME,EndPos,S,E):- 
   writeq(expr_with_text2(Out,DCG,O,StartPos,EndPos,S,E)),nl,
   get_nal_with_comments(O,_Grabber,Out,M,ME),!.

%expr_with_text(Out,DCG,O,S,E):- 
%   call(DCG,S,E) -> append(S,Some,E) -> get_nal_with_comments(O,Some,Out,S,E),!.

get_nal_with_comments(O,_,O,_,_):- compound(O),functor(O,'$COMMENT',_),!.
get_nal_with_comments(O,Txt,with_text(O,Str),S,_E):-append(Txt,_,S),!,text_to_string(Txt,Str).
%file_nal_with_comments(O,with_text(O,Txt),S,E):- copy_until_tail(S,Copy),text_to_string_safe(Copy,Txt),!.


:- thread_local(t_l:sreader_options/2).


a_nal_test("'the detective claims that tim lives in graz"):- !.

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
<<,  %,$1,sunglasses) --> own> ==> <$1 --> [aggressive]>>.
'the third deponent claims, that he has seen tom with sunglasses on:
<,  %,{tom},sunglasses) --> own>.
'the teacher claims, that people who are aggressive tend to be murders
<<$1 --> [aggressive]> ==> <$1 --> murder>>.
'the second deponent claims, that if the person lives in Graz, he is surely the murder
<<$1 --> (/,livingIn,_,{graz})> ==> <$1 --> murder>>.
'who is the murder? 
<{?who} --> murder>?
''outputMustContain('<{tom} --> murder>. %1.00;0.73%')").



test_nal:- forall(a_nal_test(Test),test_nal(Test)).


:- use_module(library(dcg/basics)).

% try_reader_test(Test):- is_stream(Test), !, \+ is_compound(Test), open_string(Test,Stream), try_reader_test(Stream).
test_nal(Test):- call_nal('dmsg',Test,Out),dmsg(Out).


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

read_nal_term(In,Expr):- 
 notrace(( is_stream(In), 
  remove_pending_buffer_codes(In,Codes), 
  read_codes_from_pending_input(In,Text), Text\==[])), !,
  call_cleanup(parse_nal_ascii(Text,Expr),
    append_buffer_codes(In,Codes)).
read_nal_term(Text,Expr):- 
 notrace(( =( ascii_,In),
  remove_pending_buffer_codes(In,Codes))),   
  call_cleanup(parse_nal_ascii(Text,Expr),
    append_buffer_codes(In,Codes)).

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

/*
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
*/

:- fixup_exports.
