#ifndef _PROC_HOLDER_H
#define _PROC_HOLDER_H
#include "ast.h"
#include "proc_holder.h"
#include "code.h"

extern void proc_initialize();

// 1 for registering procedure
extern void proc_register();

// 1 for returning code for all procedure
extern void proc_return(code_seq procDecl);

#endif
