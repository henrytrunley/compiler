(* ML Declarations *)
type lexresult = Token.t

let eof () = Token.Eof (0,0)
%%
(* Lex definitions *)
digits=[0-9]+
%%
(* Regular expressions and actions *)
if	=> (Token.If(yypos,yypos+2));
[a-z][a-z0-9]*	=> (Token.Id(yytext, yypos, yypos + size yytext))
{digits}	=> (Token.Num(Int.fromstring yytext, yypos, yypos + size yytext))
({digits}\.[0-9]*)|([0-9]*\.{digits})	=> (Token.Real(Float.fromstring yytext, yypos, yypos + size yytext))
