# Lexical syntax

## Input format
### Source encoding
Each source file is interpreted as a sequence of Unicode characters encoded in
UTF-8. It is an error if the file is not valid UTF-8.

## Whitespace
Whitespace in Pion includes spaces, tabs, and newlines. It is used to separate tokens in the source code.
```
Whitespace = " " | "\t" | "\n";
```

## Comments
Comments are ignored by the compiler.

```
Comment = LineComment | BlockComment
LineComment = "//" (Character - Newline)* Newline ;
BlockComment = "/*" BlockComment "*/"
```

## Identifiers
Identifiers in Pion are sequences of letters, digits, and underscores and
dashes, and must start with a letter. They are case-sensitive. For example,
`myVariable`, `count123`, and `another_identifier` are all valid identifiers.

```
Identifier = IdentiferStart IdentifierContunue*
IdentifierStart = "_" | Letter
IdentifierContinue = "_" | Letter | Digit

Letter = "A" | "B" | ... | "Z" | "a" | "b" | ... | "z" ;
Digit = "0" | "1" | ... | "9" ;
```

## Integer Literals
Integer literals in Pion are sequences of digits. They can be written in decimal
(base 10), hexadecimal (base 16), or binary (base 2). For example, 123, 0x7B,
and `0b1111011` all represent the same integer.

## Keywords
Keywords are reserved words that have special meanings in Pion. They cannot be used as identifiers. 
```
Keyword = "do" | "else" | "false" | "forall" | "fun" | "if" | "let" | "match" | "rec" | "then" | "true"
```