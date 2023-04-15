// proc_holder.c
#include <stdlib.h>
#include "ast.h"
#include "proc_holder.h"
#include "code.h"

node *createNode(int data)
{
  node *newNode = malloc(sizeof(node));
  newNode->data = data;
  newNode->next = NULL;

  return newNode;
}

list *createList()
{
  list *newList = malloc(sizeof(list));
  newList->head = NULL;
  newList->tail = NULL;

  return newList;
}

void insert(list *listy, int data)
{
  node *curr;

  if (listy->head == NULL)
  {
    listy->head = listy->tail = createNode(data);
  }
  else
  {
    listy->tail->next = createNode(data);
    listy->tail = listy->tail->next;
  }
  
}

node *destroy_list(node *head)
{
  if (head == NULL)
    return NULL;
  destroy_list(head->next);
  free(head);
  return NULL;
}

list *destroy_linked_list(list *listy)
{
  if (listy == NULL)
    return NULL;
  destroy_list(listy->head);
  free(listy);
  return NULL;
}

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

