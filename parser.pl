
/******************************************************************************/
/* Main entry point                                                           */
/******************************************************************************/
lab2(File, Result) :-
    read_in(File, L),
    lexer(L, Tokens),
    parser(Tokens, Result).

parseall :- tell('parser.out'),   
    write('Testing OK programs'), nl, nl,

    parsefiles(['testfiles/testok1.pas', 'testfiles/testok2.pas', 'testfiles/testok3.pas',
                'testfiles/testok4.pas', 'testfiles/testok5.pas', 'testfiles/testok6.pas',
                'testfiles/testok7.pas']), 
                
    write('Testing a-z programs'), nl, nl, 
    parsefiles(['testfiles/testa.pas', 'testfiles/testb.pas',
                'testfiles/testc.pas', 'testfiles/testd.pas', 'testfiles/teste.pas',
                'testfiles/testf.pas', 'testfiles/testg.pas', 'testfiles/testh.pas',
                'testfiles/testi.pas', 'testfiles/testj.pas', 'testfiles/testk.pas',
                'testfiles/testl.pas', 'testfiles/testm.pas', 'testfiles/testn.pas',
                'testfiles/testo.pas', 'testfiles/testp.pas', 'testfiles/testq.pas',
                'testfiles/testr.pas', 'testfiles/tests.pas', 'testfiles/testt.pas',
                'testfiles/testu.pas', 'testfiles/testv.pas', 'testfiles/testw.pas',
                'testfiles/testx.pas', 'testfiles/testy.pas', 'testfiles/testz.pas']),
        
    write('Testing fun programs'), nl, nl,
    parsefiles(['testfiles/fun1.pas', 'testfiles/fun2.pas', 'testfiles/fun3.pas',
                'testfiles/fun4.pas', 'testfiles/fun5.pas']),
                
    write('Testing sem programs'), nl, nl,          
    parsefiles(['testfiles/sem1.pas','testfiles/sem2.pas', 'testfiles/sem3.pas',
              'testfiles/sem4.pas',
              'testfiles/sem5.pas']),
    told.

parsefiles([]).
parsefiles([H|T]) :-
    write('Testing '), write(H), nl,
    read_in(H, L),   
    write(L), nl,
    lexer(L, Tokens),
    write(Tokens), nl,
    parser(Tokens, _),
    write(H), write(' end of parse'), nl, nl,
    parsefiles(T).

/******************************************************************************/
/* Reader - from Clocksin & Mellish (modified for Lab 2)                     */
/******************************************************************************/
    
read_in(File,[W|Ws]) :- see(File), get0(C), 
                        readword(C, W, C1), restsent(W, C1, Ws), nl, seen.

restsent(W, _, [])         :- W = -1.
restsent(W, _, [])         :- lastword(W).
restsent(_, C, [W1 | Ws ]) :- readword(C, W1, C1), restsent(W1, C1, Ws).

readword(C, W, _) :- C = -1, W = C.                    /* EOF handling */

readword(58, W, C2) :- get0(C1), readwordaux(58, W, C1, C2). /* := handling */

readword(C, W, C2) :-
   char_type(C, digit),
   get0(C1),
   ( char_type(C1, alpha) -> (name(W, [C]), C2 = C1)
   ; restword(C1, Cs, C2), name(A, [C|Cs]), atom_string(W, A) ).

readword(C, W, C1) :- single_character(C), name(W, [C]), get0(C1).

readword(C, W, C2) :-
   in_word(C, NewC),
   get0(C1),
   restword(C1, Cs, C2),
   name(W, [NewC|Cs]).

readword(_, W, C2) :- get0(C1), readword(C1, W, C2).

readwordaux(C, W, C1, C2) :- C1 = 61, name(W, [C, C1]), get0(C2).
readwordaux(C, W, C1, C2) :- C1 \= 61, name(W, [C]), C1 = C2.

restword(C, [NewC|Cs], C2) :-
   in_word(C, NewC),
   get0(C1),
   restword(C1, Cs, C2).
restword(C, [], C).

single_character(40). /* ( */
single_character(41). /* ) */
single_character(42). /* * */
single_character(43). /* + */
single_character(44). /* , */
single_character(59). /* ; */
single_character(58). /* : */
single_character(61). /* = */
single_character(46). /* . */
single_character(45). /* - */

in_word(C, C) :- C>96, C<123.             /* a b ... z */
in_word(C, L) :- C>64, C<91, L is C+32.   /* A B ... Z */
in_word(C, C) :- C>47, C<58.              /* 0 ... 9 */

lastword('.').

/******************************************************************************/
/* Lexer                                                                      */
/******************************************************************************/
lexer([], []).
lexer([H|T], [F|S]) :- match(H, F), lexer(T, S).

match('program', 256).
match('input', 257).
match('output', 258).
match('var', 259).
match('integer', 260).
match('begin', 261).
match('end', 262).
match('boolean', 263).
match('real', 264).
match(':=', 271).
match('(', 40).
match(')', 41).
match('*', 42).
match('+', 43).
match(',', 44).
match('.', 46).
match(':', 58).
match(';', 59).

match(W, 270) :- atom(W), atom_codes(W, [C|R]), char_type(C, alpha), forall(member(X, R), char_type(X, alnum)).
match(W, 272) :- ( atom(W) -> atom_codes(W, L)
                 ; integer(W) -> number_codes(W, L) ),
                 forall(member(C, L), char_type(C, digit)).
match(-1, 275). % EOF
match(_, 273). % undefined

/******************************************************************************/
/* Parser - Using Token Codes                                                 */
/******************************************************************************/

parser(Tokens, Res) :-
    ( prog(Tokens, Res), Res = [] ->
        write('Parse OK!'), nl
    ;
        write('Parse Fail!'), nl
    ).



/******************************************************************************/
/* Grammar Rules in Definite Clause Grammar form                              */
/* This the set of productions, P, for this grammar                           */
/* This is a slightly modified from of the Pascal Grammar for Lab 2 Prolog    */
/******************************************************************************/

prog --> prog_head, var_part, stat_part.

/******************************************************************************/
/* Program Header                                                             */
/******************************************************************************/
prog_head --> [256], id, [40], [257], [44], [258], [41], [59].

id --> [270].
number --> [272].

/******************************************************************************/
/* Var_part                                                                   */
/******************************************************************************/
var_part --> [259], var_dec_list.

var_dec_list --> var_dec, [59], var_dec_list.
var_dec_list --> var_dec, [59].

var_dec --> id_list, [58], type.

id_list --> id.
id_list --> id, [44], id_list.

type --> [260].
type --> [264].
type --> [263].

/******************************************************************************/
/* Stat part                                                                  */
/******************************************************************************/
stat_part --> [261], stat_list, [262], [46].

stat_list --> stat.
stat_list --> stat, [59], stat_list.

stat --> assign_stat.
assign_stat --> id, [271], expr.

expr --> term.
expr --> term, [43], expr.

term --> factor.
term --> factor, [42], term.

factor --> id.
factor --> number.
factor --> [40], expr, [41].

/******************************************************************************/
/* End of Parser                                                              */
/******************************************************************************/
