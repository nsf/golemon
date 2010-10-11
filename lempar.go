/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is included that follows the "include" declaration
** in the input grammar file. */
package main

import (
	"io"
	"fmt"
)

%%
/* Next is all token values, in a form suitable for use by makeheaders.
** This section will be null unless lemon is run with the -m switch.
*/
/* 
** These constants (all generated automatically by the parser generator)
** specify the various kinds of tokens (terminals) that the parser
** understands. 
**
** Each symbol here is a terminal symbol in the grammar.
*/
%%

/* The next thing included is series of defines which control
** various aspects of the generated parser.
**    YYCODETYPE         is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 terminals
**                       and nonterminals.  "int" is used otherwise.
**    YYNOCODE           is a number of type YYCODETYPE which corresponds
**                       to no legal terminal or nonterminal number.  This
**                       number is used to fill in empty slots of the hash 
**                       table.
**    YYFALLBACK         If defined, this indicates that one or more tokens
**                       have fall-back values which should be used if the
**                       original value of the token will not parse.
**    YYACTIONTYPE       is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 rules and
**                       states combined.  "int" is used otherwise.
**    ParseTOKENTYPE     is the data type used for minor tokens given 
**                       directly to the parser from the tokenizer.
**    YYMINORTYPE        is the data type used for all minor tokens.
**                       This is typically a union of many types, one of
**                       which is ParseTOKENTYPE.  The entry in the union
**                       for base tokens is called "yy0".
**    YYSTACKDEPTH       is the maximum depth of the parser's stack.  If
**                       zero the stack is dynamically sized using realloc()
**    ParseARG_SDECL     A static variable declaration for the %extra_argument
**    ParseARG_PDECL     A parameter declaration for the %extra_argument
**    ParseARG_STORE     Code to store %extra_argument into yypParser
**    ParseARG_FETCH     Code to extract %extra_argument from yypParser
**    YYNSTATE           the combined number of states.
**    YYNRULE            the number of rules in the grammar
**    YYERRORSYMBOL      is the code number of the error symbol.  If not
**                       defined, then do no error processing.
*/
%%
const (
	YY_NO_ACTION     = YYNSTATE + YYNRULE + 2
	YY_ACCEPT_ACTION = YYNSTATE + YYNRULE + 1
	YY_ERROR_ACTION  = YYNSTATE + YYNRULE
)

/* Next are the tables used to determine what action to take based on the
** current state and lookahead token.  These tables are used to implement
** functions that take a state number and lookahead value and return an
** action integer.  
**
** Suppose the action integer is N.  Then the action is determined as
** follows
**
**   0 <= N < YYNSTATE                  Shift N.  That is, push the lookahead
**                                      token onto the stack and goto state N.
**
**   YYNSTATE <= N < YYNSTATE+YYNRULE   Reduce by rule N-YYNSTATE.
**
**   N == YYNSTATE+YYNRULE              A syntax error has occurred.
**
**   N == YYNSTATE+YYNRULE+1            The parser accepts its input.
**
**   N == YYNSTATE+YYNRULE+2            No such action.  Denotes unused
**                                      slots in the yy_action[] table.
**
** The action table is constructed as a single large table named yy_action[].
** Given state S and lookahead X, the action is computed as
**
**      yy_action[ yy_shift_ofst[S] + X ]
**
** If the index value yy_shift_ofst[S]+X is out of range or if the value
** yy_lookahead[yy_shift_ofst[S]+X] is not equal to X or if yy_shift_ofst[S]
** is equal to YY_SHIFT_USE_DFLT, it means that the action is not in the table
** and that yy_default[S] should be used instead.  
**
** The formula above is for computing the action when the lookahead is
** a terminal symbol.  If the lookahead is a non-terminal (as occurs after
** a reduce action) then the yy_reduce_ofst[] array is used in place of
** the yy_shift_ofst[] array and YY_REDUCE_USE_DFLT is used in place of
** YY_SHIFT_USE_DFLT.
**
** The following are the tables generated in this section:
**
**  yy_action[]        A single table containing all actions.
**  yy_lookahead[]     A table containing the lookahead for each entry in
**                     yy_action.  Used to detect hash collisions.
**  yy_shift_ofst[]    For each state, the offset into yy_action for
**                     shifting terminals.
**  yy_reduce_ofst[]   For each state, the offset into yy_action for
**                     shifting non-terminals after a reduce.
**  yy_default[]       Default action for each state.
*/
%%

/* The next table maps tokens into fallback tokens.  If a construct
** like the following:
** 
**      %fallback ID X Y Z.
**
** appears in the grammar, then ID becomes a fallback token for X, Y,
** and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
** but it does not parse, the type of the token is changed to ID and
** the parse is retried before an error is thrown.
*/
var yyFallback = []YYCODETYPE{
%%
}

/* The following structure represents a single element of the
** parser's stack.  Information stored includes:
**
**   +  The state number for the parser at this level of the stack.
**
**   +  The value of the token stored at this level of the stack.
**      (In other words, the "major" token.)
**
**   +  The semantic value stored at this level of the stack.  This is
**      the information used by the action routines in the grammar.
**      It is sometimes called the "minor" token.
*/
type yyStackEntry struct {
	stateno YYACTIONTYPE
	major YYCODETYPE
	minor YYMINORTYPE
}

/* The state of the parser is completely contained in an instance of
** the following structure */
type yyParser struct {
	idx int
	errcnt int
	stack []yyStackEntry

	traceWriter io.Writer
	tracePrompt string
}

/*
** Turn parser tracing on by giving a stream to which to write the trace
** and a prompt to preface each trace message.  Tracing is turned off
** by making either argument NULL 
**
** Inputs:
** <ul>
** <li> A FILE* to which trace output should be written.
**      If NULL, then tracing is turned off.
** <li> A prefix string written at the beginning of every
**      line of trace output.  If NULL, then tracing is
**      turned off.
** </ul>
**
** Outputs:
** None.
*/
func (p *yyParser) Trace(traceWriter io.Writer, tracePrompt string) {
	p.traceWriter = traceWriter
	p.tracePrompt = tracePrompt
	if p.traceWriter == nil {
		p.tracePrompt = ""
	}
}

/* For tracing shifts, the names of all terminals and nonterminals
** are required.  The following table supplies these names */
var yyTokenName = []string{
%%
}

/* For tracing reduce actions, the names of all rules are required.
*/
var yyRuleName = []string{
%%
}

/*
** Try to increase the size of the parser stack.
*/
func (p *yyParser) growStack() {
	n := len(p.stack)
	s := make([]yyStackEntry, n*2+100)
	copy(s, p.stack)
	p.stack = s

	// TODO(nsf): add 'if debug' for dead code elimination
	if p.traceWriter != nil {
		fmt.Fprintf(p.traceWriter, "%sStack grows to %d entries!\n",
			    p.tracePrompt, len(p.stack))
	}
}

/* 
** This function allocates a new parser.
** The only argument is a pointer to a function which works like
** malloc.
**
** Inputs:
** A pointer to the function used to allocate memory.
**
** Outputs:
** A pointer to a parser.  This pointer is used in subsequent calls
** to Parse and ParseFree.
*/
func NewParser() *yyParser {
	p := new(yyParser)
	p.idx = -1
	p.errcnt = 0
	p.stack = make([]yyStackEntry, 100)
	return p
}

/* The following function deletes the value associated with a
** symbol.  The symbol can be either a terminal or nonterminal.
** "yymajor" is the symbol code, and "yypminor" is a pointer to
** the value.
*/
func (p *yyParser) destructor(major YYCODETYPE, minor *YYMINORTYPE) {
	// TODO(nsf): ParseARG_FETCH
	switch major {
%%
	}
}

/*
** Pop the parser's stack once.
**
** If there is a destructor routine associated with the token which
** is popped from the stack, then call it.
**
** Return the major token number for the symbol popped.
*/
func (p *yyParser) popParserStack() YYCODETYPE {
	if p.idx < 0 {
		return 0
	}

	top := &p.stack[p.idx]
	major := top.major

	// TODO(nsf): add 'if debug' for dead code elimination
	if p.traceWriter != nil {
		fmt.Fprintf(p.traceWriter, "%sPopping %s\n",
			    p.tracePrompt, yyTokenName[major])
	}
	p.destructor(major, &top.minor)
	p.idx--
	return major
}

/* 
** Deallocate and destroy a parser.  Destructors are all called for
** all stack elements before shutting the parser down.
**
** Inputs:
** <ul>
** <li>  A pointer to the parser.  This should be a pointer
**       obtained from ParseAlloc.
** <li>  A pointer to a function used to reclaim memory obtained
**       from malloc.
** </ul>
*/
func (p *yyParser) Dispose() {
	for p.idx >= 0 {
		p.popParserStack()
	}
}

/*
** Find the appropriate action for a parser given the terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
func (p *yyParser) findShiftAction(lookahead YYCODETYPE) YYACTIONTYPE {
	stateno := p.stack[p.idx].stateno

	if stateno > YY_SHIFT_COUNT {
		return yy_default[stateno]
	}

	i := int(yy_shift_ofst[stateno])
	if i == YY_SHIFT_USE_DFLT {
		return yy_default[stateno]
	}

	// assert(lookahead != YYNOCODE)
	i += int(lookahead)
	if i >= 0 && i < len(yy_action) && yy_lookahead[i] == lookahead {
		return yy_action[i]
	}
	if lookahead > 0 {
		if int(lookahead) < len(yyFallback) {
			fallback := yyFallback[lookahead]
			if fallback != 0 {
				// TODO(nsf): add 'if debug' for dead code elimination
				if p.traceWriter != nil {
					fmt.Fprintf(p.traceWriter, "%sFALLBACK %s => %s\n",
						    p.tracePrompt,
						    yyTokenName[lookahead],
						    yyTokenName[fallback])
				}
				return p.findShiftAction(fallback)
			}
		}
		if YYWILDCARD >= 0 {
			var wildcard int = YYWILDCARD
			j := i - int(lookahead) + wildcard

			if j >= 0 && j < len(yy_action) && int(yy_lookahead[j]) == wildcard {
				// TODO(nsf): add 'if debug' for dead code elimination
				if p.traceWriter != nil {
					fmt.Fprintf(p.traceWriter, "%sWILDCARD %s => %s\n",
						    p.tracePrompt,
						    yyTokenName[lookahead],
						    yyTokenName[wildcard])
				}
				return yy_action[j]
			}
		}
	}
	return yy_default[stateno]
}

/*
** Find the appropriate action for a parser given the non-terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
func (p *yyParser) findReduceAction(stateno YYACTIONTYPE, lookahead YYCODETYPE) YYACTIONTYPE {
	// assert(stateno <= YY_REDUCE_MAX)
	var i int = int(yy_reduce_ofst[stateno])
	// assert(i != YY_REDUCE_USE_DFLT)
	// assert(lookahead != YYNOCODE)
	i += int(lookahead)
	// assert(i >= 0 && i < len(yy_action))
	// assert(yy_lookahead[i] == lookahead)
	return yy_action[i]
}

/*
** The following routine is called if the stack overflows.
** TODO(nsf): We have a dynamic stack, it can't overflow? remove this method?
*/
func (p *yyParser) stackOverflow(pminor *YYMINORTYPE) {
	// TODO(nsf): ParseARG_FETCH
	p.idx--
	// TODO(nsf): add 'if debug' for dead code elimination
	if p.traceWriter != nil {
		fmt.Fprintf(p.traceWriter, "%sStack Overflow!\n",
			    p.tracePrompt)
	}
	for p.idx >= 0 {
		p.popParserStack()
	}
%%
	// TODO(nsf): ParseARG_STORE
}

/*
** Perform a shift action.
*/
func (p *yyParser) shift(newState YYACTIONTYPE, major YYCODETYPE, pminor *YYMINORTYPE) {
	p.idx++
	if p.idx >= len(p.stack) {
		p.growStack()

		// TODO(nsf): can't happen? dynamic stack doesn't overflow
		if p.idx >= len(p.stack) {
			p.stackOverflow(pminor)
			return
		}
	}

	top := &p.stack[p.idx]
	top.stateno = newState
	top.major = major
	top.minor = *pminor

	// TODO(nsf): add 'if debug' for dead code elimination
	if p.traceWriter != nil {
		fmt.Fprintf(p.traceWriter, "%sShift %d\n",
			    p.tracePrompt, newState)
		fmt.Fprintf(p.traceWriter, "%sStack:",
			    p.tracePrompt)
		for i := 1; i <= p.idx; i++ {
			fmt.Fprintf(p.traceWriter, " %s", yyTokenName[p.stack[i].major])
		}
		fmt.Fprintf(p.traceWriter, "\n")
	}
}

/* The following table contains information about every rule that
** is used during the reduce.
*/
type ruleInfoEntry struct {
	lhs YYCODETYPE
	nrhs byte
}

var yyRuleInfo = []ruleInfoEntry{
%%
}

/*
** Perform a reduce action and the shift that must immediately
** follow the reduce.
** Use 'yyp' instead of 'p', because the namespace of this function is visible
** in rule actions, we don't want clashes.
*/
func (yyp *yyParser) reduce(ruleno YYACTIONTYPE) {
	var yygoto YYCODETYPE
	var yyact YYACTIONTYPE
	var yygotominor YYMINORTYPE
	var yysize int

	// TODO(nsf): add 'if debug' for dead code elimination
	if yyp.traceWriter != nil && ruleno >= 0 && int(ruleno) < len(yyRuleName) {
		fmt.Fprintf(yyp.traceWriter, "%sReduce [%s].\n",
			    yyp.tracePrompt, yyRuleName[ruleno])
	}

	// these will be used in rule actions
	yystack := yyp.stack
	yyidx := yyp.idx

	switch ruleno {
%%
	}

	yygoto = yyRuleInfo[ruleno].lhs
	yysize = int(yyRuleInfo[ruleno].nrhs)
	yyp.idx -= yysize
	yyact = yyp.findReduceAction(yyp.stack[yyp.idx].stateno, yygoto)
	if yyact < YYNSTATE {
		// TODO(nsf): add 'if debug' for dead code elimination
		yyp.shift(yyact, yygoto, &yygotominor)
		/* TODO(nsf): enable this 'if !debug'
		if yysize > 0 {
			yyp.idx++
			yymsp = &yyp.stack[yyp.idx]
			yymsp.stateno = yyact
			yymsp.major = yygoto
			yymsp.minor = yygotominor
		} else {
			yyp.shift(yyact, yygoto, &yygotominor)
		}
		*/
	} else {
		// assert(yyact == YYNSTATE + YYNRULE + 1)
		yyp.accept()
	}
}

/*
** The following code executes when the parse fails
*/
func (p *yyParser) parseFailed() {
	// TODO(nsf): ParseARG_FETCH

	// TODO(nsf): add 'if debug' for dead code elimination
	if p.traceWriter != nil {
		fmt.Fprintf(p.traceWriter, "%sFail!\n", p.tracePrompt)
	}

	for p.idx >= 0 {
		p.popParserStack()
	}
%%
	// TODO(nsf): ParseARG_STORE
}

/*
** The following code executes when a syntax error first occurs.
*/
func (p *yyParser) syntaxError(major YYCODETYPE, minor YYMINORTYPE) {
	// TODO(nsf): ParseARG_FETCH

	// TODO(nsf): #define TOKEN (minor.yy0)
%%
	// TODO(nsf): ParseARG_STORE
}

/*
** The following is executed when the parser accepts
*/
func (p *yyParser) accept() {
	// TODO(nsf): ParseARG_FETCH

	// TODO(nsf): add 'if debug' for dead code elimination
	if p.traceWriter != nil {
		fmt.Fprintf(p.traceWriter, "%sAccept!\n", p.tracePrompt)
	}
	for p.idx >= 0 {
		p.popParserStack()
	}
%%
	// TODO(nsf): ParseARG_STORE
}

/* The main parser program.
** The first argument is a pointer to a structure obtained from
** "ParseAlloc" which describes the current state of the parser.
** The second argument is the major token number.  The third is
** the minor token.  The fourth optional argument is whatever the
** user wants (and specified in the grammar) and is available for
** use by the action routines.
**
** Inputs:
** <ul>
** <li> A pointer to the parser (an opaque structure.)
** <li> The major token number.
** <li> The minor token number.
** <li> An option argument of a grammar-specified type.
** </ul>
**
** Outputs:
** None.
*/
func (p *yyParser) Parse(major YYCODETYPE, minor ParseTOKENTYPE, arg ...interface{}) {
	var (
		minorunion YYMINORTYPE
		act YYACTIONTYPE
		endofinput bool
		errorhit bool
	)

	if p.idx < 0 {
		p.idx = 0
		p.errcnt = -1
		p.stack[0].stateno = 0
		p.stack[0].major = 0
	}

	minorunion = minor
	endofinput = major == 0

	// TODO(nsf): add 'if debug' for dead code elimination
	if p.traceWriter != nil {
		fmt.Fprintf(p.traceWriter, "%sInput %s\n",
			    p.tracePrompt,
			    yyTokenName[major])
	}
	for {
		act = p.findShiftAction(major)
		if act < YYNSTATE {
			// assert(!endofinput)
			p.shift(act, major, &minorunion)
			p.errcnt--
			major = YYNOCODE
		} else if act < YYNSTATE + YYNRULE {
			p.reduce(act - YYNSTATE)
		} else {
			// assert(act == YY_ERROR_ACTION)
			// TODO(nsf): add 'if debug' for dead code elimination
			if p.traceWriter != nil {
				fmt.Fprintf(p.traceWriter, "%sSyntax Error!\n",
					    p.tracePrompt)
			}

			if YYERRORSYMBOL >= 0 {
				var errsym int = YYERRORSYMBOL
				if p.errcnt < 0 {
					p.syntaxError(major, minorunion)
				}
				mx := int(p.stack[p.idx].major)
				if mx == errsym || errorhit {
					// TODO(nsf): add 'if debug' for dead code elimination
					if p.traceWriter != nil {
						fmt.Fprintf(p.traceWriter,
							    "%sDiscard input token %s\n",
							    p.tracePrompt,
							    yyTokenName[major])
					}
					p.destructor(major, &minorunion)
					major = YYNOCODE
				} else {
					for p.idx >= 0 && mx != errsym {
						act = p.findReduceAction(p.stack[p.idx].stateno,
									 YYCODETYPE(errsym))
						if act < YYNSTATE {
							break
						}
						p.popParserStack()
					}
					if p.idx < 0 || major == 0 {
						p.destructor(major, &minorunion)
						p.parseFailed()
						major = YYNOCODE
					} else if mx != errsym {
						var u2 YYMINORTYPE
						//u2.yy0 = ParseTOKENTYPE(nil)
						p.shift(act, YYCODETYPE(errsym), &u2)
					}
				}
				p.errcnt = 3
				errorhit = true
			} else {
				if p.errcnt <= 0 {
					p.syntaxError(major, minorunion)
				}
				p.errcnt = 3
				p.destructor(major, &minorunion)
				if endofinput {
					p.parseFailed()
				}
				major = YYNOCODE
			}
		}
		if !(major != YYNOCODE && p.idx >= 0) {
			break
		}
	}
}
