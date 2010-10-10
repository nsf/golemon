// Simple AST building example, Go-ish style.
// In order to build do the following:
// 1. build golemon
// 2. ../../lemon parser.y
// 3. 8g ast.go parser.go parser_tokens.go
// 4. 8l -o ast ast.8

package main

import "fmt"

type Expr interface {
	String() string
}

//-------------------------------------------------------------------------
// NumExpr
//-------------------------------------------------------------------------
type NumExpr int

func (n NumExpr) String() string { return fmt.Sprintf("%d", int(n)) }

//-------------------------------------------------------------------------
// BinExpr
//-------------------------------------------------------------------------
type BinExpr struct {
	tok int
	lhs Expr
	rhs Expr
}

var toktostr = map[int]string {
	PLUS: "+",
	MINUS: "-",
	TIMES: "*",
	DIVIDE: "/",
}

func (b *BinExpr) String() string {
	return fmt.Sprintf("(%s%s%s)", b.lhs.String(), toktostr[b.tok], b.rhs.String())
}

var AST Expr

// let's make an AST

func main() {
	p := NewParser()
	p.Parse(INTEGER, 5)
	p.Parse(PLUS, 0)
	p.Parse(INTEGER, 10)
	p.Parse(TIMES, 0)
	p.Parse(INTEGER, 4)
	p.Parse(0, 0)
	p.Dispose()

	fmt.Println(AST)

	p = NewParser()
	p.Parse(INTEGER, 5)
	p.Parse(TIMES, 0)
	p.Parse(INTEGER, 10)
	p.Parse(PLUS, 0)
	p.Parse(INTEGER, 4)
	p.Parse(0, 0)
	p.Dispose()

	fmt.Println(AST)
}
