#ifndef _PROC_HOLDER_H
#define _PROC_HOLDER_H
#include "ast.h"
#include "proc_holder.h"
#include "code.h"

typedef struct node{
  int data;
  struct node *next;
}node;

typedef struct list{
  node *head;
  node *tail;
}list;

extern node *createNode(int data);

extern list *createList();

extern void insert(list *listy, int data);

extern node *destroy_list(node *head);

extern list *destroy_linked_list(list *listy);

extern void proc_initialize();

// 1 for registering procedure
extern void proc_register();

// 1 for returning code for all procedure
extern void proc_return(code_seq procDecl);

#endif
