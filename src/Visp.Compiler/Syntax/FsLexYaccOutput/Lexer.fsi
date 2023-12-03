
/// Rule token
val token: args: LexArgs -> skip: bool -> lexbuf: LexBuffer<char> -> token
/// Rule tokenStream
val tokenStream: args: LexArgs -> skip: bool -> lexbuf: LexBuffer<char> -> token
/// Rule singleQuoteString
val singleQuoteString: sargs: LexerStringArgs -> skip: bool -> lexbuf: LexBuffer<char> -> token
/// Rule tripleQuoteString
val tripleQuoteString: sargs: LexerStringArgs -> skip: bool -> lexbuf: LexBuffer<char> -> token
