type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val commentDepth = ref 0
val commentStartPos = ref 0;

val stringContents = ref ""
val stringStartPos = ref 0
val inString = ref false

fun eof() = 
  let 
    val pos = hd(!linePos)
  in 
    if !commentDepth > 0 then
      ErrorMsg.error (!commentStartPos) "unclosed comment"
    else if !inString then
      ErrorMsg.error (!stringStartPos) "unclosed string"
    else ();
    Tokens.EOF(pos,pos) 
  end


%%
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
alpha=[a-zA-Z];
ws=[ \t\r\n];
%s COMMENT STRING MULTILINE;
%%
<INITIAL,COMMENT, MULTILINE> \n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL, MULTILINE> {ws}+ => (continue());


<INITIAL> type => (Tokens.TYPE(yypos,yypos+4));
<INITIAL> var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL> function => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL> break => (Tokens.BREAK(yypos,yypos+5));
<INITIAL> of => (Tokens.OF(yypos,yypos+2));
<INITIAL> end => (Tokens.END(yypos,yypos+3));
<INITIAL> in => (Tokens.IN(yypos,yypos+2));
<INITIAL> nil => (Tokens.NIL(yypos,yypos+3));
<INITIAL> let => (Tokens.LET(yypos,yypos+3));
<INITIAL> do => (Tokens.DO(yypos,yypos+2));
<INITIAL> to => (Tokens.TO(yypos,yypos+2));
<INITIAL> for => (Tokens.FOR(yypos,yypos+3));
<INITIAL> while => (Tokens.WHILE(yypos,yypos+5));
<INITIAL> else => (Tokens.ELSE(yypos,yypos+4));
<INITIAL> then => (Tokens.THEN(yypos,yypos+4));
<INITIAL> if => (Tokens.IF(yypos,yypos+2));
<INITIAL> array => (Tokens.ARRAY(yypos,yypos+5));

<INITIAL> ":=" => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL> "|" => (Tokens.OR(yypos,yypos+1));
<INITIAL> "&" => (Tokens.AND(yypos,yypos+1));
<INITIAL> ">=" => (Tokens.GE(yypos,yypos+2));
<INITIAL> ">" => (Tokens.GT(yypos,yypos+1));
<INITIAL> "<" => (Tokens.LT(yypos,yypos+1));
<INITIAL> "<=" => (Tokens.LE(yypos,yypos+2));
<INITIAL> "<>" => (Tokens.NEQ(yypos,yypos+2));
<INITIAL> "=" => (Tokens.EQ(yypos,yypos+1));
<INITIAL> "/" => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL> "*" => (Tokens.TIMES(yypos,yypos+1));
<INITIAL> "-" => (Tokens.MINUS(yypos,yypos+1));
<INITIAL> "+" => (Tokens.PLUS(yypos,yypos+1));
<INITIAL> "." => (Tokens.DOT(yypos,yypos+1));
<INITIAL> "}" => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL> "{" => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL> "]" => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL> "[" => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL> ")" => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL> "(" => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL> ";" => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL> ":" => (Tokens.COLON(yypos,yypos+1));
<INITIAL> ","	=> (Tokens.COMMA(yypos,yypos+1));

<INITIAL> [0-9]+	=> (Tokens.INT(valOf(Int.fromString(yytext)),yypos,yypos+size(yytext)));
<INITIAL> {alpha}({alpha}|[0-9]|"_")* => (Tokens.ID(yytext,yypos,yypos+size(yytext)));

<INITIAL> "\"" => (YYBEGIN STRING; stringContents := ""; stringStartPos := yypos; inString := true; continue());

<STRING> \n => (ErrorMsg.error (!stringStartPos) "unclosed string"; lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<STRING> \\n => (stringContents := !stringContents ^ "\n"; continue());
<STRING> \\t => (stringContents := !stringContents ^ "\t"; continue());
<STRING> \\\\ => (stringContents := !stringContents ^ "\\"; continue());
<STRING> \\\" => (stringContents := !stringContents ^ "\""; continue());
<STRING> \\[0-9]{3} => (let
                          val n = valOf (Int.fromString (String.extract (yytext,1,NONE)))
                        in
                          if n < 128 then
                            stringContents := !stringContents ^ String.str (Char.chr n)
                          else ErrorMsg.error yypos ("illegal ascii escape " ^ yytext)
                        end; 
                        continue());
<STRING> \\\^{alpha} => (let
                           val c = valOf (Char.fromString (String.extract (yytext,2,NONE)))
                          in
                           if ord c > 63 andalso ord c < 96 then
                             let
                               val newc = Char.chr (ord c - 64)
                             in
                               stringContents := !stringContents ^ String.str newc
                             end
                           else ErrorMsg.error yypos ("illegal ascii escape " ^ yytext)
                         end;
                         continue());
<STRING> \\. => (ErrorMsg.error yypos ("illegal ascii escape " ^ yytext); continue());


<STRING> "\"" => (YYBEGIN INITIAL; inString := false; Tokens.STRING(!stringContents,!stringStartPos,yypos+size(!stringContents)));

<STRING> \\ => (YYBEGIN MULTILINE; continue());
<MULTILINE> \\ => (YYBEGIN STRING; continue());

<STRING> . => (stringContents := !stringContents ^ yytext; continue());


<INITIAL> "/*" => (YYBEGIN COMMENT; commentDepth := !commentDepth+1; commentStartPos := yypos; continue());
<COMMENT> "/*" => (commentDepth := !commentDepth+1; continue());
<COMMENT> "*/" => (commentDepth := !commentDepth-1; if !commentDepth = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT> . => (continue());

.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

