// proc_holder.c
#include "ast.h"
#include "proc_holder.h"
#include "code.h"

// initialize
void proc_initialize()
{

}
// register procedure
void proc_register()
{

}

// return code for all procedure
void proc_return(code_seq procDecl)
{
	procDecl = code_seq_add_to_end(procDecl, code_rtn());
}

