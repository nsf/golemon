%token_type { int }

%left PLUS MINUS.
%left DIVIDE TIMES.

%include {
	//import "fmt"
}

%syntax_error {
	fmt.Println("Syntax error!")
}

program ::= expr(A). { AST = A }

%type expr { Expr }
expr(A) ::= expr(B) MINUS expr(C). { A = &BinExpr{MINUS, B, C} }
expr(A) ::= expr(B) PLUS expr(C). { A = &BinExpr{PLUS, B, C} }
expr(A) ::= expr(B) TIMES expr(C). { A = &BinExpr{TIMES, B, C} }
expr(A) ::= expr(B) DIVIDE expr(C). { A = &BinExpr{DIVIDE, B, C} }

expr(A) ::= INTEGER(B). { A = NumExpr(B) }
