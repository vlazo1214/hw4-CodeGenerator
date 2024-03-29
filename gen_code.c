// By: Vincent Lazo and Christian Manuel
// programmer's note: mainly referred to Enhanced Abstract Syntax

// Credits to Dr. Leavens for the initial stubs
/* $Id: gen_code.c,v 1.10 2023/03/30 21:28:07 leavens Exp $ */
#include "utilities.h"
#include "gen_code.h"
#include "proc_holder.h"
#include "ast.h"

// global variables for procedures
// procDecls is the ptr to the whole list of decls, procDecl is an indexing
// ptr that helps concatenate the decls
int procDeclsSize;
list *addressIndex;
code_seq procDecls;
code_seq procDecl;

// Initialize the code generator
void gen_code_initialize()
{
    // initialize procedure global
	procDecls = code_seq_empty();
	procDecl = code_seq_empty();
	procDeclsSize = 0;
	addressIndex = createList();
}

code_seq gen_code_program(AST *prog)
{
	code_seq ret = code_seq_empty();
	
	// <program> ::= { cds }{ vds }{ pds } stmt
	code_seq block = gen_code_block(prog);
	int procSize = code_seq_size(procDecls);

	if (procSize > 0)
	{
		ret = code_seq_add_to_end(ret, code_jmp(procSize + 1));
		ret = code_seq_concat(ret, procDecls);
	}

	ret = code_seq_add_to_end(ret, code_inc(LINKS_SIZE));
	ret = code_seq_concat(ret, block);
	ret = code_seq_add_to_end(ret, code_hlt());


	// fix labels
	code_seq_fix_labels(procDecls);

	addressIndex = destroy_linked_list(addressIndex);

	return ret;
}

code_seq gen_code_block(AST *prog)
{
    /* design:
       [code to make space for the static link, INC 1]
       [code to allocate space for all the vars declared.]
       [code for the statement]
       HLT
     */
    code_seq ret = code_seq_empty();
	
	code_seq cds = gen_code_constDecls(prog->data.program.cds);
    code_seq vds = gen_code_varDecls(prog->data.program.vds);
	code_seq stmts = gen_code_stmt(prog->data.program.stmt);

	gen_code_procDecls(prog->data.program.pds);

	// // code_fix_labels
	// code_seq_fix_labels(procDecls);	

	// ret = code_seq_concat(ret, procDecls);
	ret = code_seq_concat(ret, cds);
	ret = code_seq_concat(ret, vds);
	ret = code_seq_concat(ret, stmts);
    return ret;
}

// generate code for the declarations in cds
code_seq gen_code_constDecls(AST_list cds)
{
    code_seq ret = code_seq_empty();
    while (!ast_list_is_empty(cds))
	{
		ret = code_seq_concat(ret, gen_code_constDecl(ast_list_first(cds)));
		cds = ast_list_rest(cds);
    }
    return ret;
}

// singular const decl
// recall: <const-decl> ::= const <name> = <number>
code_seq gen_code_constDecl(AST *cd)
{
	return code_seq_singleton(code_lit(cd->data.const_decl.num_val));
}

// generate code for the declarations in vds
code_seq gen_code_varDecls(AST_list vds)
{
    code_seq ret = code_seq_empty();
    while (!ast_list_is_empty(vds))
	{
		ret = code_seq_concat(ret, gen_code_varDecl(ast_list_first(vds)));
		vds = ast_list_rest(vds);
    }
    return ret;
}

// generate code for the var declaration vd
// recall: <var-decl> ::= var <name>
code_seq gen_code_varDecl(AST *vd)
{
    return code_seq_singleton(code_inc(1));
}

void gen_code_procDecls(AST_list pds)
{
	if (!ast_list_is_empty(pds))
	{
		while (!ast_list_is_empty(pds))
		{
			gen_code_procDecl(ast_list_first(pds));
			procDecls = code_seq_concat(procDecls, procDecl);
		
			// set procDecl to the next available index
			procDecl = code_seq_last_elem(procDecl);
			procDecl->next = code_seq_empty();
			procDecl = procDecl->next;
		
			pds = ast_list_rest(pds);

			procDeclsSize++;
		}

		// procDecls = code_seq_add_to_end(procDecls, code_rtn());

	}
}

// proc0.pl0: procedure p; skip;
// procDecls = |  p ( {skip;}, at address: label )  | -> NULL
// procDecl = <program>, label

void gen_code_procDecl(AST *pd)
{
	AST *tempAST = pd->data.proc_decl.block;
	code_seq tempBlock = gen_code_block(tempAST);

	// concat tempBlock
	procDecl = code_seq_concat(procDecl, tempBlock);

	// INC to pop call if > 0
	// consolidate
	if (ast_list_size(tempAST->data.program.cds) > 0)
	{
		int negativeCDSize = -1 * ast_list_size(tempAST->data.program.cds);
		procDecl = code_seq_add_to_end(procDecl, code_inc(negativeCDSize));
	}
	if (ast_list_size(tempAST->data.program.vds) > 0)
	{
		int negativeVDSize = -1 * ast_list_size(tempAST->data.program.vds);
		procDecl = code_seq_add_to_end(procDecl, code_inc(negativeVDSize));
	}

	// 2 0
	procDecl = code_seq_add_to_end(procDecls, code_rtn());

	// procDecl->lab = label_create();
	// label_set(procDecl->lab, pd->data.proc_decl.lab->addr);
}

// generate code for the statement
code_seq gen_code_stmt(AST *stmt)
{
    switch (stmt->type_tag)
	{
		case assign_ast:
			return gen_code_assignStmt(stmt);
			break;
		case call_ast:
			return gen_code_callStmt(stmt);
			break;
		case begin_ast:
			return gen_code_beginStmt(stmt);
			break;
		case if_ast:
			return gen_code_ifStmt(stmt);
			break;
		case while_ast:
			return gen_code_whileStmt(stmt);
			break;	
		case read_ast:
			return gen_code_readStmt(stmt);
			break;
		case write_ast:
			return gen_code_writeStmt(stmt);
			break;
		case skip_ast:
			return gen_code_skipStmt(stmt);
			break;
		default:
			bail_with_error("Bad AST passed to gen_code_stmt!");
			// The following should never execute
			return code_seq_empty();
    }
}

// generate code for assignment statement
code_seq gen_code_assignStmt(AST *stmt)
{
    /* design of code seq:
       [get fp for the variable on top of stack]
       [get value of expression on top of stack]
       STO([offset for the variable])
     */
    unsigned int outLevels = stmt->data.assign_stmt.ident->data.ident.idu->levelsOutward;
    unsigned int ofst = stmt->data.assign_stmt.ident->data.ident.idu->attrs->loc_offset;
    
	code_seq ret = code_compute_fp(outLevels);
    ret = code_seq_concat(ret, gen_code_expr(stmt->data.assign_stmt.exp));
    ret = code_seq_add_to_end(ret, code_sto(ofst));
    return ret;
}

	// printf("in call\n");
	// fflush(stdout);

	// int i, procDeclSize = code_seq_size(procDecls);
	// code *index;
	// address identAddr = stmt->data.call_stmt.ident->data.ident.idu->attrs->lab->addr;
	// label *identLabel = stmt->data.call_stmt.ident->data.ident.idu->attrs->lab;
    // code_seq ret = code_seq_empty();

	// printf("set inital ident\n");
	// fflush(stdout);

	// label_set(identLabel, identAddr);

	// printf("init complete\n");
	// fflush(stdout);

	// index = code_seq_first(procDecls);

	// printf("procDeclSize: %d\n", procDeclSize);
	// fflush(stdout);

	// printf("abt to enter loop\n");
	// fflush(stdout);

	// if (procDeclSize > 0)
	// {
	// 	for (i = 0; i < procDeclSize; i++)
	// 	{
	// 		printf("iteration: %d\n", i);
	// 		fflush(stdout);
			
	// 		if (index->lab == identLabel)
	// 		{
	// 			label_set(index->lab, identLabel->addr);
	// 			break;
	// 		}

	// 		index = code_seq_rest(procDecls);
	// 	}
	// }
	// else
	// {
	// 	printf("abt to set proc label\n");
	// 	fflush(stdout);

	// 	label_set(index->lab, identLabel->addr);
	// }

	// printf("done setting\n");
	// fflush(stdout);

	// ret = code_seq_add_to_end(ret, code_cal(index->lab));
// <call-stmt> ::= call <ident>
code_seq gen_code_callStmt(AST *stmt)
{
	stmt->data.call_stmt.ident->data.ident.idu->attrs->lab->is_set = true;
    address identAddr = label_read(stmt->data.call_stmt.ident->data.ident.idu->attrs->lab);
    label *identLabel = label_create();

    label_set(identLabel, identAddr);

    code_seq ret = code_seq_empty();
    ret = code_seq_add_to_end(ret, code_cal(identLabel));
    // codecode_cal(label *lab)
    // code_seq_fix_labels(code_seq cs)

	return ret;
}

// save the static link (surronding scope's BP) on stack
// code_seq ret = code_seq_singleton(code_pbp());
// // set the BP to SP-1
// ret = code_seq_add_to_end(ret, code_psp());
// ret = code_seq_add_to_end(ret, code_lit(1));
// ret = code_seq_add_to_end(ret, code_sub());
// RBP (restore break pointer) does not exist in our grammar, do we need to substitute?
// ret = code_seq_add_to_end(ret, code_rbp());
// add code for all the statements
// generate code for begin statement
code_seq gen_code_beginStmt(AST *stmt)
{
    /* design of code_seq
        [save old BP on stack, PBP]
	[adjust the BP]
        [allocate variables declared]
	[concatenated code for each stmt]
	[if there are variables, pop them off the stack]
        [RBP]
     */
	code_seq ret = code_seq_empty();
    AST_list stmts = stmt->data.begin_stmt.stmts;

    while (!ast_list_is_empty(stmts))
	{
		ret = code_seq_concat(ret, gen_code_stmt(ast_list_first(stmts)));
		stmts = ast_list_rest(stmts);
    }

    // restore the old BP
    // ret = code_seq_add_to_end(ret, code_rbp());
    return ret;
}

// from lab || generate code for the statement
code_seq gen_code_ifStmt(AST *stmt)
{
    /* design:
        [code for pushing the condition on top of stack]
	JPC 2
        JMP [around the body]
        [code for the body]
     */
    code_seq condc = gen_code_cond(stmt->data.if_stmt.cond);
    code_seq thenc = gen_code_stmt(stmt->data.if_stmt.thenstmt);
	code_seq elsec = gen_code_stmt(stmt->data.if_stmt.elsestmt);
    code_seq ret = code_seq_add_to_end(condc, code_jpc(2));

    ret = code_seq_add_to_end(ret, code_jmp(code_seq_size(thenc)+2));
    ret = code_seq_concat(ret, thenc);
	ret = code_seq_add_to_end(ret, code_jmp(code_seq_size(elsec)+1));
    ret = code_seq_concat(ret, elsec);
    return ret;
}

// from lab || generate code for while stmt
code_seq gen_code_whileStmt(AST *stmt)
{
	code_seq condc = gen_code_cond(stmt->data.while_stmt.cond);
    code_seq bodyc = gen_code_stmt(stmt->data.while_stmt.stmt);
    code_seq ret = code_seq_first(condc);
	unsigned int condSize = code_seq_size(condc);

	ret = code_seq_add_to_end(ret, code_jpc(2));
	ret = code_seq_add_to_end(ret, code_jmp(code_seq_size(bodyc) + 2));
	ret = code_seq_concat(ret, bodyc);
	ret = code_seq_add_to_end(ret, code_jmp(-1 * (code_seq_size(bodyc) + condSize + 2)));

	return ret;
}

code_seq gen_code_cond(AST *cond)
{
	switch (cond->type_tag)
	{
		case odd_cond_ast:
			return gen_code_odd_cond(cond);
			break;
		case bin_cond_ast:
			return gen_code_bin_cond(cond);
			break;
		default:
			bail_with_error("gen_code_cond passed bad AST!");
			// The following should never execute
			return code_seq_empty();
			break;
	}
}

code_seq gen_code_odd_cond(AST *cond)
{
	code_seq ret = gen_code_expr(cond->data.odd_cond.exp);
	ret = code_seq_add_to_end(ret, code_lit(2));
	ret = code_seq_add_to_end(ret, code_mod());
	return ret;
}

// should be self-explanatory || generate code for the statement
code_seq gen_code_readStmt(AST *stmt)
{
    /* design:
       [code to put the fp for the variable on top of stack]
       CHI
       STO [(variable offset)]
     */
    id_use *idu = stmt->data.read_stmt.ident->data.ident.idu;
    code_seq ret = code_compute_fp(idu->levelsOutward);
    ret = code_seq_add_to_end(ret, code_chi());
    ret = code_seq_add_to_end(ret, code_sto(idu->attrs->loc_offset));
    return ret;
}

// should be self-explanatory || generate code for the statement
code_seq gen_code_writeStmt(AST *stmt)
{
    /* design:
       [code to put the exp's value on top of stack
       CHO
     */
    code_seq ret = gen_code_expr(stmt->data.write_stmt.exp);
	ret = code_seq_add_to_end(ret, code_cho());
    return ret;
}

code_seq gen_code_skipStmt(AST *stmt)
{
	return code_seq_singleton(code_nop());
}

// !! generate code for the expresion
code_seq gen_code_expr(AST *exp)
{
    switch (exp->type_tag)
	{
		case number_ast:
			return gen_code_number_expr(exp);
			break;
		case ident_ast:
			return gen_code_ident_expr(exp);
			break;
		case bin_expr_ast:
			return gen_code_bin_expr(exp);
			break;
		default:
			bail_with_error("gen_code_expr passed bad AST!");
			// The following should never execute
			return code_seq_empty();
			break;
    }
}

// generate code for binary condition (relational operators)
code_seq gen_code_bin_cond(AST *exp)
{
    /* design:
        [code to push left exp's value on top of stack]
	[code to push right exp's value on top of stack]
	[instruction that implements the operation op]
    */
    code_seq ret = gen_code_expr(exp->data.bin_expr.leftexp);
    ret = code_seq_concat(ret, gen_code_expr(exp->data.bin_expr.rightexp));
    switch (exp->data.bin_cond.relop)
	{
		case eqop:
			ret = code_seq_add_to_end(ret, code_eql());
			return ret;
			break;
		case neqop:
			ret = code_seq_add_to_end(ret, code_neq());
			return ret;
			break;
		case ltop:
			ret = code_seq_add_to_end(ret, code_lss());
			return ret;
			break;
		case leqop:
			ret = code_seq_add_to_end(ret, code_leq());
			return ret;
			break;
		case gtop:
			ret = code_seq_add_to_end(ret, code_gtr());
			return ret;
			break;
		case geqop:
			ret = code_seq_add_to_end(ret, code_geq());
			return ret;
			break;
		default:
			bail_with_error("gen_code_bin_cond passed AST with bad op!");
			// The following should never execute
			return code_seq_empty();
    }
}

// generate code for binary expression (arithmetic operators)
code_seq gen_code_bin_expr(AST *exp)
{
    /* design:
        [code to push left exp's value on top of stack]
	[code to push right exp's value on top of stack]
	[instruction that implements the operation op]
    */
    code_seq ret = gen_code_expr(exp->data.bin_expr.leftexp);
    ret = code_seq_concat(ret, gen_code_expr(exp->data.bin_expr.rightexp));
    switch (exp->data.bin_expr.arith_op)
	{
		case addop:
			ret = code_seq_add_to_end(ret, code_add());
			return ret;
			break;
		case subop:
			ret = code_seq_add_to_end(ret, code_sub());
			return ret;
			break;
		case multop:
			ret = code_seq_add_to_end(ret, code_mul());
			return ret;
			break;
		case divop:
			ret = code_seq_add_to_end(ret, code_div());
			return ret;
			break;
		default:
			bail_with_error("gen_code_bin_expr passed AST with bad op!");
			// The following should never execute
			return code_seq_empty();
    }
}

// generate code for the ident expression (ident)
code_seq gen_code_ident_expr(AST *ident)
{
    /* design:
       [code to load fp for the variable]
       LOD [offset for the variable]
     */
    id_use *idu = ident->data.ident.idu;
    lexical_address *la = lexical_address_create(idu->levelsOutward, idu->attrs->loc_offset);
    return code_load_from_lexical_address(la);
}

// generate code for the number expression (num)
code_seq gen_code_number_expr(AST *num)
{
    return code_seq_singleton(code_lit(num->data.number.value));
}
