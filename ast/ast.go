package ast

import "clojurium/token"

type Node interface {
	TokenLiteral() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

// Program Node is a root of any programm
type Program struct {
	Statements []Statement
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	} else {
		return ""
	}
}

// LET STATEMENT
// let x = 5;
// let y = 1 + 3;
// let hello = hi("some text");
type LetStatement struct {
	Token token.Token // token.LET token
	Name  *Identifier // Any identifier
	Value Expression  // Expression bound to identifier, we should somehow calculate it
}

func (ls *LetStatement) statementNode()       {}
func (ls *LetStatement) TokenLiteral() string { return ls.Token.Literal }

// RETURN STATEMENT
// return 5;
// return (5 + 10);
type ReturnStatement struct {
	Token       token.Token // the `return` token
	ReturnValue Expression
}

func (rs *ReturnStatement) statementNode()       {}
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }

// IDENTIFIER
type Identifier struct {
	Token token.Token // token.IDENT token
	Value string
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
