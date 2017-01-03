package lexer

type Lexer struct {
	input        string
	position     int  // curr pos in input(points to current char)
	readPosition int  // after current char
	ch           byte // char we are working with now
}

func New(input string) *Lexer {
	l := &Lexer{input: input}
	return l
}
