#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "header.h"
#include "symbolTable.h"
// This file is for reference only, you are not required to follow the implementation. //
// You only need to check for errors stated in the hw4 document. //
int g_anyErrorOccur = 0;

DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2);
void processProgramNode(AST_NODE *programNode);
void processDeclarationNode(AST_NODE* declarationNode);
void declareIdList(AST_NODE* typeNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize);
void declareFunction(AST_NODE* returnTypeNode);
void processDeclDimList(AST_NODE* variableDeclDimList, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize);
void processTypeNode(AST_NODE* typeNode);
void processBlockNode(AST_NODE* blockNode);
void processStmtNode(AST_NODE* stmtNode);
void processGeneralNode(AST_NODE *node);
void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode);
void checkWhileStmt(AST_NODE* whileNode);
void checkForStmt(AST_NODE* forNode);
void checkAssignmentStmt(AST_NODE* assignmentNode);
void checkIfStmt(AST_NODE* ifNode);
void checkWriteFunction(AST_NODE* functionCallNode);
void checkFunctionCall(AST_NODE* functionCallNode);
void processExprRelatedNode(AST_NODE* exprRelatedNode);
void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter);
void checkReturnStmt(AST_NODE* returnNode);
void processExprNode(AST_NODE* exprNode);
void processVariableLValue(AST_NODE* idNode);
void processVariableRValue(AST_NODE* idNode);
void processConstValueNode(AST_NODE* constValueNode);
void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue);
void evaluateExprValue(AST_NODE* exprNode);


typedef enum ErrorMsgKind
{
    SYMBOL_IS_NOT_TYPE,
    SYMBOL_REDECLARE,
    SYMBOL_UNDECLARED,
    NOT_FUNCTION_NAME,
    TRY_TO_INIT_ARRAY,
    EXCESSIVE_ARRAY_DIM_DECLARATION,
    RETURN_ARRAY,
    VOID_VARIABLE,
    TYPEDEF_VOID_ARRAY,
    PARAMETER_TYPE_UNMATCH,
    TOO_FEW_ARGUMENTS,
    TOO_MANY_ARGUMENTS,
    RETURN_TYPE_UNMATCH,
    INCOMPATIBLE_ARRAY_DIMENSION,
    NOT_ASSIGNABLE,
    NOT_ARRAY,
    IS_TYPE_NOT_VARIABLE,
    IS_FUNCTION_NOT_VARIABLE,
    STRING_OPERATION,
    ARRAY_SIZE_NOT_INT,
    ARRAY_SIZE_NEGATIVE,
    ARRAY_SUBSCRIPT_NOT_INT,
    PASS_ARRAY_TO_SCALAR,
    PASS_SCALAR_TO_ARRAY
} ErrorMsgKind;

void printErrorMsgSpecial (AST_NODE *node1, char *name2, ErrorMsgKind errorMsgKind) {
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node1->linenumber);
    /*
    switch(errorMsgKind)
    {
    default:
        printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind)\n");
        break;
    }
    */
}


void printErrorMsg (AST_NODE *node, ErrorMsgKind errorMsgKind) {
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    /*
    switch(errorMsgKind)
    {
        printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind)\n");
        break;
    }
    */
}


void semanticAnalysis (AST_NODE *root) {
    processProgramNode(root);
}


DATA_TYPE getBiggerType (DATA_TYPE dataType1, DATA_TYPE dataType2) {
    if(dataType1 == FLOAT_TYPE || dataType2 == FLOAT_TYPE) {
        return FLOAT_TYPE;
    } else {
        return INT_TYPE;
    }
}


void processProgramNode (AST_NODE *programNode) {
    initializeSymbolTable();
    AST_NODE *child = programNode->child;
    while (child) {
        processDeclarationNode(programNode->child);
        child = child->rightSibling;
    }
    symbolTableEnd();
}

void processDeclarationNode(AST_NODE* declarationNode)
{
    if(declarationNode->nodeType == VARIABLE_DECL_LIST_NODE) {
        AST_NODE *child = declarationNode->child;
        while(child) {
            declareIdList(child, child->semantic_value.declSemanticValue.kind, 0);
            child = child->rightSibling;
        }
    }
    else
        declareFunction(declarationNode);
}


void processTypeNode (AST_NODE *idNodeAsType) {
    SymbolTableEntry* entry = retrieveSymbol(idNodeAsType->semantic_value.identifierSemanticValue.identifierName);
    while(entry != NULL) {
        if(entry->attribute->attributeKind == TYPE_ATTRIBUTE)
            break;
        entry = entry->nextInHashTrain;
    }

    if(entry == NULL) {
        //TODO: type not found
    }
    else {
        idNodeAsType->semantic_value.identifierSemanticValue.symbolTableEntry = entry;
    }
}


void declareIdList (AST_NODE *declarationNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize) {
    
}

void checkAssignOrExpr (AST_NODE *assignOrExprRelatedNode) {
}

void checkWhileStmt (AST_NODE *whileNode) {
}


void checkForStmt (AST_NODE *forNode) {
}


void checkAssignmentStmt (AST_NODE *assignmentNode) {
}


void checkIfStmt(AST_NODE *ifNode) {
}

void checkWriteFunction(AST_NODE *functionCallNode) {
}

void checkFunctionCall (AST_NODE *functionCallNode) {
}

void checkParameterPassing (Parameter *formalParameter, AST_NODE *actualParameter) {
}


void processExprRelatedNode (AST_NODE *exprRelatedNode) {
}

void getExprOrConstValue (AST_NODE *exprOrConstNode, int *iValue, float *fValue) {
}

void evaluateExprValue (AST_NODE *exprNode) {
}


void processExprNode (AST_NODE *exprNode) {
}


void processVariableLValue (AST_NODE *idNode) {
}

void processVariableRValue (AST_NODE *idNode) {
}


void processConstValueNode (AST_NODE *constValueNode) {
}


void checkReturnStmt (AST_NODE *returnNode) {
}


// xatier
// block
//     : decl_list stmt_list
//     | stmt_list
//     | decl_list
void processBlockNode (AST_NODE *blockNode) {
    AST_NODE *child = blockNode->child;
    while (child) {
        switch (child->nodeType) {
            case VARIABLE_DECL_LIST_NODE:
                processDeclDimList(child);
                break;
            case STMT_LIST_NODE:
                processStmtNode(child);
                break;
            default:
                break;
        }

        child = child->rightSibling;
    }
}


// xatier
// stmt
//     : { block }
//     | while ( relop_expr ) { block }
//     | for ( assign_expr_list ; relop_expr_list ; assign_expr_list ) { block }
//     | var_ref = relop_expr ;
//     | if ( relop_expr ) { block }
//     | if ( relop_expr ) ;
//     | if ( relop_expr ) { block } else stmt
//     | id ( relop_expr_list ) ;                 // function call
//     | ;
//     | return ;
//     | return relop_expr ;
void processStmtNode(AST_NODE* stmtNode)
{
    AST_NODE *child = stmtNode->child;
    while (child) {
        if (child->nodeType == BLOCK_NODE) {
            processBlockNode(child);
        }
        else {
            switch (child->semantic_value.stmtSemanticValue.kind) {
                case WHILE_STMT:
                    checkWhileStmt(child);
                    break;
                case FOR_STMT:
                    checkForStmt(child);
                    break;
                case ASSIGN_STMT:
                    checkAssignmentStmt(child);
                    break;
                case IF_STMT:
                    checkIfStmt(child);
                    break;
                case FUNCTION_CALL_STMT:
                    checkFunctionCall(child);
                    break;
                case RETURN_STMT:
                    checkReturnStmt(child);
                    break;
                default:
                    break;
            }
        }

        child = child->rightSibling;
    }
}


void processGeneralNode (AST_NODE *node) {
}

void processDeclDimList (AST_NODE *idNode, TypeDescriptor *typeDescriptor, int ignoreFirstDimSize) {
}


void declareFunction (AST_NODE *declarationNode) {
}
