/* $Id: gen_code_stubs.c,v 1.8 2023/03/29 15:42:08 leavens Exp leavens $ */
#include "utilities.h"
#include "gen_code.h"

// Initialize the code generator
void gen_code_initialize()
{
    // Replace the following with your implementation
    bail_with_error("gen_code_initialize not implemented yet!");
}

// Generate code for the given AST
code_seq gen_code_program(AST *prog)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_block not implemented yet!");
    return code_seq_empty();
}

// generate code for blk
code_seq gen_code_block(AST *blk)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_block not implemented yet!");
    return code_seq_empty();
}

// generate code for the declarations in cds
code_seq gen_code_constDecls(AST_list cds)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_constDecls not implemented yet!");
    return code_seq_empty();
}

// generate code for the const declaration cd
code_seq gen_code_constDecl(AST *cd)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_constDecl not implemented yet!");
    return code_seq_empty();
}

// generate code for the declarations in vds
code_seq gen_code_varDecls(AST_list vds)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_varDecls not implemented yet!");
    return code_seq_empty();
}

// generate code for the var declaration vd
code_seq gen_code_varDecl(AST *vd)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_varDecl not implemented yet!");
    return code_seq_empty();
}

// generate code for the declarations in pds
void gen_code_procDecls(AST_list pds)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_procDecls not implemented yet!");
}

// generate code for the procedure declaration pd
void gen_code_procDecl(AST *pd)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_procDecl not implemented yet!");
}

// generate code for the statement
code_seq gen_code_stmt(AST *stmt)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_stmt not implemented yet!");
    return code_seq_empty();
}

// generate code for the statement
code_seq gen_code_assignStmt(AST *stmt)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_assignStmt not implemented yet!");
    return code_seq_empty();
}

// generate code for the statement
code_seq gen_code_callStmt(AST *stmt)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_callStmt not implemented yet!");
    return code_seq_empty();
}

// generate code for the statement
code_seq gen_code_beginStmt(AST *stmt)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_beginStmt not implemented yet!");
    return code_seq_empty();
}

// generate code for the statement
code_seq gen_code_ifStmt(AST *stmt)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_ifStmt not implemented yet!");
    return code_seq_empty();
}

// generate code for the statement
code_seq gen_code_whileStmt(AST *stmt)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_whileStmt not implemented yet!");
    return code_seq_empty();
}

// generate code for the statement
code_seq gen_code_readStmt(AST *stmt)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_readStmt not implemented yet!");
    return code_seq_empty();
}

// generate code for the statement
code_seq gen_code_writeStmt(AST *stmt)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_writeStmt not implemented yet!");
    return code_seq_empty();
}

// generate code for the statement
code_seq gen_code_skipStmt(AST *stmt)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_skipStmt not implemented yet!");
    return code_seq_empty();
}

// generate code for the condition
code_seq gen_code_cond(AST *cond)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_cond not implemented yet!");
    return code_seq_empty();
}

// generate code for the condition
code_seq gen_code_odd_cond(AST *cond)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_odd_cond not implemented yet!");
    return code_seq_empty();
}

// generate code for the condition
code_seq gen_code_bin_cond(AST *cond)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_bin_cond not implemented yet!");
    return code_seq_empty();
}

// generate code for the expresion
code_seq gen_code_expr(AST *exp)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_expr not implemented yet!");
    return code_seq_empty();
}

// generate code for the expression (exp)
code_seq gen_code_bin_expr(AST *exp)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_bin_expr not implemented yet!");
    return code_seq_empty();
}

// generate code for the ident expression (ident)
code_seq gen_code_ident_expr(AST *ident)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_ident_expr not implemented yet!");
    return code_seq_empty();
}

// generate code for the number expression (num)
code_seq gen_code_number_expr(AST *num)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_number_expr not implemented yet!");
    return code_seq_empty();
}
