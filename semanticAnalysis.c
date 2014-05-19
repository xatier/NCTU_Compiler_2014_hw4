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
void processDeclarationNode(AST_NODE *declarationNode);
void declareIdList(AST_NODE *typeNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize);
void declareFunction(AST_NODE *returnTypeNode);
void processDeclDimList(AST_NODE *variableDeclDimList, TypeDescriptor *typeDescriptor, int ignoreFirstDimSize);
void processTypeNode(AST_NODE *typeNode);
void processBlockNode(AST_NODE *blockNode);
void processStmtNode(AST_NODE *stmtNode);
void processGeneralNode(AST_NODE *node);
void checkAssignOrExpr(AST_NODE *assignOrExprRelatedNode);
void checkWhileStmt(AST_NODE *whileNode);
void checkForStmt(AST_NODE *forNode);
void checkAssignmentStmt(AST_NODE *assignmentNode);
void checkIfStmt(AST_NODE *ifNode);
void checkWriteFunction(AST_NODE *functionCallNode);
void checkFunctionCall(AST_NODE *functionCallNode);
void processExprRelatedNode(AST_NODE *exprRelatedNode);
void checkParameterPassing(Parameter *formalParameter, AST_NODE *actualParameter);
void checkReturnStmt(AST_NODE *returnNode);
void processExprNode(AST_NODE *exprNode);
void processVariableLValue(AST_NODE *idNode);
void processVariableRValue(AST_NODE *idNode);
void processConstValueNode(AST_NODE *constValueNode);
void getExprOrConstValue(AST_NODE *exprOrConstNode, int *iValue, float *fValue);
void evaluateExprValue(AST_NODE *exprNode);


typedef enum ErrorMsgKind {
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

void printErrorMsgSpecial (AST_NODE *node1, char *name, ErrorMsgKind errorMsgKind) {
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node1->linenumber);

    switch (errorMsgKind) {
        case SYMBOL_UNDECLARED:
            printf("ID <%s> undeclared.\n", name);
            break;
        case SYMBOL_REDECLARE:
            printf("ID <%s> redeclared.\n", name);
            break;
        case TOO_MANY_ARGUMENTS:
            printf("too many arguments to function <%s>.\n", name);
            break;
        case TOO_FEW_ARGUMENTS:
            printf("too few arguments to function <%s>.\n", name);
            break;
        case PASS_ARRAY_TO_SCALAR:
            printf("Array <%s> passed to scalar parameter <%s>.\n", node1->semantic_value.identifierSemanticValue.identifierName, name);
            break;
        case PASS_SCALAR_TO_ARRAY:
            printf("Scalar <%s> passed to array parameter <%s>.\n", node1->semantic_value.identifierSemanticValue.identifierName, name);
            break;
        case SYMBOL_IS_NOT_TYPE:
            printf("Type <%s> is not a valid type", name);
            break;
        default:
            printf("Unhandled case in void printErrorMsgSpecial()\n");
            break;
    }
}


void printErrorMsg (AST_NODE *node, ErrorMsgKind errorMsgKind) {
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    switch (errorMsgKind) {
        case RETURN_TYPE_UNMATCH:
            puts("Incompatible return type.");
            break;
        case INCOMPATIBLE_ARRAY_DIMENSION:
            puts("Incompatible array dimensions.");
            break;
        case ARRAY_SUBSCRIPT_NOT_INT:
            printf("Array subscript is not an integer");
            break;
        default:
            printf("Unhandled case in void printErrorMsg()\n");
            break;
    }
}


void semanticAnalysis (AST_NODE *root) {
    processProgramNode(root);
}


DATA_TYPE getBiggerType (DATA_TYPE dataType1, DATA_TYPE dataType2) {
    if (dataType1 == FLOAT_TYPE || dataType2 == FLOAT_TYPE) {
        return FLOAT_TYPE;
    }
    else {
        return INT_TYPE;
    }
}

// xatier
// program
//     : global_decl_list
//     |
//     ;
//
//
// global_decl_list
//     : global_decl_list global_decl
//     | global_decl
//     ;

void processProgramNode (AST_NODE *programNode) {
    initializeSymbolTable();
    AST_NODE *child = programNode->child;
    while (child) {
        processDeclarationNode(child);
        child = child->rightSibling;
    }
    symbolTableEnd();
}


// xatier
// global_decl
//     : decl_list function_decl
//     | function_decl
//     ;

void processDeclarationNode(AST_NODE *declarationNode)
{
    AST_NODE *child = declarationNode->child;
    AST_NODE *current = declarationNode;
    if (declarationNode->nodeType == VARIABLE_DECL_LIST_NODE) {
        while (current) {
            processDeclarationNode(current->child);
            current = current->rightSibling;
        }
    }
    else if (declarationNode->semantic_value.declSemanticValue.kind == FUNCTION_DECL) {
        declareFunction(declarationNode);
    }
    else if (declarationNode->semantic_value.declSemanticValue.kind == VARIABLE_DECL) {
        declareIdList(declarationNode, declarationNode->semantic_value.declSemanticValue.kind, 0);
    }
}

// xatier
// type
//     : INT
//     | FLOAT
//     ;
void processTypeNode (AST_NODE *idNodeAsType) {
    // take type as an IDNode, only allow "int", "float" and "void"
    // which is a NORMAL_ID
    // jyhsu: typedef is allowed to be used, this won't work

    char *typeName = idNodeAsType->semantic_value.identifierSemanticValue.identifierName;

    // is it a NORMAL_ID?
    if (idNodeAsType->semantic_value.identifierSemanticValue.kind != NORMAL_ID) {
        printErrorMsgSpecial(idNodeAsType, typeName, SYMBOL_IS_NOT_TYPE);
    }

    // is is one of three?
    if (!(strcmp(typeName, "int") != 0 ||
        strcmp(typeName, "float") != 0 ||
        strcmp(typeName, "void")  != 0)) {
        printErrorMsgSpecial(idNodeAsType, typeName, SYMBOL_IS_NOT_TYPE);
    }
}


// xatier: I can't understand this Orz
// second param is to make sure we delare a var or use typedef
// third param is only for func declaration
void declareIdList (AST_NODE *declarationNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize) {
    AST_NODE *id = declarationNode->child->rightSibling;
    processTypeNode(declarationNode->child);
    SymbolTableEntry *nameCheck;
    SymbolAttribute *attribute;

    if (declarationNode->child->semantic_value.identifierSemanticValue.symbolTableEntry == NULL) //check type is declared
        return;

    while (id != NULL) {
        nameCheck = retrieveSymbol(id->semantic_value.identifierSemanticValue.identifierName); //check var or type name is undeclared

        switch (isVariableOrTypeAttribute) {
            case VARIABLE_ATTRIBUTE: //declare a var
                while (nameCheck != NULL) {
                    if (nameCheck->attribute->attributeKind == VARIABLE_ATTRIBUTE)
                        break;
                    nameCheck = nameCheck->sameNameInOuterLevel;
                }

                if (nameCheck != NULL) {
                    // id redeclared
                    printErrorMsgSpecial(id, id->semantic_value.identifierSemanticValue.identifierName, SYMBOL_REDECLARE);
                    break;
                }

                attribute = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
                attribute->attributeKind = isVariableOrTypeAttribute;

                if (id->semantic_value.identifierSemanticValue.kind == ARRAY_ID) { //declare array
                    TypeDescriptor *arrayType = (TypeDescriptor *)malloc(sizeof(TypeDescriptor));
                    processDeclDimList(id, arrayType, ignoreArrayFirstDimSize);
                    if(arrayType->properties.arrayProperties.dimension == 0)
                        break;

                    arrayType->properties.arrayProperties.elementType = declarationNode->child->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.dataType;
                    attribute->attr.typeDescriptor = arrayType;
                }
                else //declare normal var
                    attribute->attr.typeDescriptor = declarationNode->child->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;

                id->semantic_value.identifierSemanticValue.symbolTableEntry = enterSymbol(id->semantic_value.identifierSemanticValue.identifierName, attribute);

                if (id->semantic_value.identifierSemanticValue.kind == WITH_INIT_ID) { //forgot to remake this part...
                    if(id->nodeType == EXPR_NODE)
                        processExprNode(id->child);
                    else if(id->nodeType == STMT_NODE)
                        checkFunctionCall(id->child);
                    else if(id->nodeType == IDENTIFIER_NODE)
                        processVariableRValue(id->child);
                    else
                        processConstValueNode(id->child);
                }

                break;

            case TYPE_ATTRIBUTE: //declare a type
                while (nameCheck != NULL) {
                    if(nameCheck->attribute->attributeKind == TYPE_ATTRIBUTE)
                        break;
                    nameCheck = nameCheck->sameNameInOuterLevel;
                }

                if (nameCheck != NULL) {
                    // id redeclared
                    printErrorMsgSpecial(id, id->semantic_value.identifierSemanticValue.identifierName, SYMBOL_REDECLARE);
                    break;
                }

                attribute->attr.typeDescriptor = declarationNode->child->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
                id->semantic_value.identifierSemanticValue.symbolTableEntry = enterSymbol(id->semantic_value.identifierSemanticValue.identifierName, attribute);
                break;
            default:
                //just in case
                break;
        }
        id = id->rightSibling;
    }
}

void checkAssignOrExpr (AST_NODE *assignOrExprRelatedNode) {
}

void checkWhileStmt (AST_NODE *whileNode) {
    processExprNode(whileNode->child);
    processBlockNode(whileNode->child->rightSibling);
}


// xatier
// stmt
//     | for ( assign_expr_list ; relop_expr_list ; assign_expr_list ) { block }
void checkForStmt (AST_NODE *forNode) {
    AST_NODE *assign1 = forNode->child->child;
    AST_NODE *expr    = forNode->child->rightSibling->child;
    AST_NODE *assign2 = forNode->child->rightSibling->rightSibling->child;
    AST_NODE *block   = forNode->child->rightSibling->rightSibling->rightSibling;

    while (assign1 != NULL) {
        checkAssignmentStmt(assign1);
        assign1 = assign1->rightSibling;
    }

    while (expr != NULL) {
        processExprNode(expr);
        expr = expr->rightSibling;
    }

    while (assign2 != NULL) {
        checkAssignmentStmt(assign2);
        assign2 = assign2->rightSibling;
    }

    processBlockNode(block);
}


// xatier: assignment is an expression ... Orz
// this function plays with `assign_expr_list` ...?
// jyhsu: you use it in processStmtNode so i use it
void checkAssignmentStmt (AST_NODE *assignmentNode) {
    AST_NODE *lhs = assignmentNode->child;
    AST_NODE *rhs = assignmentNode->child->rightSibling;
    processVariableRValue(lhs);

    if(rhs->nodeType == EXPR_NODE)
        processExprNode(rhs);
    else if(rhs->nodeType == IDENTIFIER_NODE)
        processVariableRValue(rhs);
    else if(rhs->nodeType == STMT_NODE)
        checkFunctionCall(rhs);
    else
        processConstValueNode(rhs);
}


// xatier
// stmt
//     | if ( relop_expr ) { block }
//     | if ( relop_expr ) ;
//     | if ( relop_expr ) { block } else stmt
void checkIfStmt (AST_NODE *ifNode) {
    // xatier: ignore the second case now
    processExprNode(ifNode->child);
    processBlockNode(ifNode->child->rightSibling);
    AST_NODE *moreElse = ifNode->child->rightSibling->rightSibling;
    if (moreElse->nodeType == STMT_NODE)
        checkIfStmt(moreElse);
    else if (moreElse->nodeType == BLOCK_NODE)
        processBlockNode(moreElse);

}

void checkWriteFunction (AST_NODE *functionCallNode) {
    SymbolTableEntry *entry;
    int formalCount, actualCount = 0;
    AST_NODE *actualParameter = functionCallNode->child->rightSibling->child;
    entry = retrieveSymbol(actualParameter->semantic_value.identifierSemanticValue.identifierName);

    while (entry != NULL) {
        if (entry->attribute->attributeKind == VARIABLE_ATTRIBUTE)
            break;
        entry = entry->sameNameInOuterLevel;
    }

    if (entry == NULL) {
        // id undeclared
        printErrorMsgSpecial(actualParameter, actualParameter->semantic_value.identifierSemanticValue.identifierName, SYMBOL_UNDECLARED);
        return;
    }

    formalCount = entry->attribute->attr.functionSignature->parametersCount;
    while (actualParameter != NULL) {
        actualCount++;
        actualParameter = actualParameter->rightSibling;
    }

    if (formalCount < actualCount) {
        // to many argu
        printErrorMsgSpecial(functionCallNode, functionCallNode->semantic_value.identifierSemanticValue.identifierName, TOO_MANY_ARGUMENTS);
    }
    else if(formalCount > actualCount) {
        // to few argu
        printErrorMsgSpecial(functionCallNode, functionCallNode->semantic_value.identifierSemanticValue.identifierName, TOO_FEW_ARGUMENTS);
    }
    else
        functionCallNode->semantic_value.identifierSemanticValue.symbolTableEntry = entry;

}

void checkFunctionCall (AST_NODE *functionCallNode) {
    checkWriteFunction(functionCallNode);
    SymbolTableEntry* formalParameter = functionCallNode->semantic_value.identifierSemanticValue.symbolTableEntry;
    if (formalParameter == NULL)
        return;

    checkParameterPassing(formalParameter->attribute->attr.functionSignature->parameterList, functionCallNode->child->rightSibling->child);
}

void checkParameterPassing (Parameter *formalParameter, AST_NODE *actualParameter) {
    while (formalParameter != NULL && actualParameter != NULL) {
        if (actualParameter->nodeType == IDENTIFIER_NODE) {
            processVariableRValue(actualParameter);
            if(actualParameter->semantic_value.identifierSemanticValue.symbolTableEntry == NULL)
                continue;

            TypeDescriptorKind actual = actualParameter->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->kind;
            TypeDescriptorKind formal = formalParameter->type->kind;
            if (actual == ARRAY_TYPE_DESCRIPTOR && formal == SCALAR_TYPE_DESCRIPTOR) {
                // array passed to scalar parameter
                printErrorMsgSpecial(actualParameter, formalParameter->parameterName, PASS_ARRAY_TO_SCALAR);
            }
            else if (actual == SCALAR_TYPE_DESCRIPTOR && formal == ARRAY_TYPE_DESCRIPTOR) {
                // scalar passed to array parameter
                printErrorMsgSpecial(actualParameter, formalParameter->parameterName, PASS_SCALAR_TO_ARRAY);
            }
        }
        else if(actualParameter->nodeType == STMT_NODE)
            checkFunctionCall(actualParameter);
        else if(actualParameter->nodeType == EXPR_NODE)
            processExprNode(actualParameter);
        else
            processConstValueNode(actualParameter);


        formalParameter = formalParameter->next;
        actualParameter = actualParameter->rightSibling;
    }
}


void processExprRelatedNode (AST_NODE *exprRelatedNode) {
}

void getExprOrConstValue (AST_NODE *exprOrConstNode, int *iValue, float *fValue) {
}

void evaluateExprValue (AST_NODE *exprNode) {
}


void processExprNode (AST_NODE *exprNode) {
    AST_NODE* left = exprNode->child;
    AST_NODE* right = exprNode->child->rightSibling;
    DATA_TYPE leftType, rightType;
    int leftConst = 0, rightConst = 0;
    exprNode->semantic_value.exprSemanticValue.isConstEval = 0;

    if(left->nodeType == EXPR_NODE) {
        processExprNode(left);
        leftType = left->dataType;
        leftConst = left->semantic_value.exprSemanticValue.isConstEval;
    }
    else if(left->nodeType == IDENTIFIER_NODE) {
        processVariableRValue(left);
        leftType = left->dataType;
    }
    else if(left->nodeType == STMT_NODE) {
        checkFunctionCall(left);
        leftType = left->child->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.functionSignature->returnType;
    }
    else {
        leftConst = 1;
        processConstValueNode(left);
        leftType = left->dataType;
    }

    if(right != NULL) { //exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION
        if(right->nodeType == EXPR_NODE) {
            processExprNode(right);
            rightType = right->dataType;
            rightConst = right->semantic_value.exprSemanticValue.isConstEval;
        }
        else if(right->nodeType == IDENTIFIER_NODE) {
            processVariableRValue(right);
            rightType = right->dataType;
        }
        else if(right->nodeType == STMT_NODE) {
            checkFunctionCall(right);
            rightType = right->child->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.functionSignature->returnType;
        }
        else {
            rightConst = 1;
            processConstValueNode(right);
            rightType = right->dataType;
        }
    }

    if(exprNode->semantic_value.exprSemanticValue.kind == UNARY_OPERATION) {
        exprNode->dataType = leftType;
        if(leftConst == 1) {
            exprNode->semantic_value.exprSemanticValue.isConstEval = 1;
        }
    }
    else{
        exprNode->dataType = getBiggerType(leftType, rightType);
        if(leftConst == 1 && rightConst == 1) {
            exprNode->semantic_value.exprSemanticValue.isConstEval = 1;
        }
    }
}


void processVariableLValue (AST_NODE *idNode) {
}

void processVariableRValue (AST_NODE *idNode) {
    SymbolTableEntry *entry = retrieveSymbol(idNode->semantic_value.identifierSemanticValue.identifierName);
    if(entry == NULL) {
        //TODO: id undeclared
        idNode->semantic_value.identifierSemanticValue.symbolTableEntry = entry;
        return;
    }

    idNode->semantic_value.identifierSemanticValue.symbolTableEntry = entry;

    if(idNode->semantic_value.identifierSemanticValue.kind == ARRAY_ID) {
        int count = 0;
        AST_NODE* dim = idNode->child;
        DATA_TYPE dimType;

        while(dim != NULL) {
            count++;
            if(dim->nodeType == EXPR_NODE) {
                processExprNode(dim);
                dimType = dim->dataType;
            }
            else if(dim->nodeType == IDENTIFIER_NODE) {
                processVariableRValue(dim);
                dimType = dim->dataType;
            }
            else if(dim->nodeType == STMT_NODE) {
                checkFunctionCall(dim);
                dimType = dim->child->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.functionSignature->returnType;
            }
            else {
                processConstValueNode(dim);
                dimType = dim->dataType;
            }

            if(dimType != INT_TYPE) {
                //TODO: Array subscript is not an integer
            }
        }

        if(count != entry->attribute->attr.typeDescriptor->properties.arrayProperties.dimension) {
            //TODO: Incompatible array dimensions
        }

        idNode->dataType = entry->attribute->attr.typeDescriptor->properties.arrayProperties.elementType;
    }
    else{
        idNode->dataType = entry->attribute->attr.typeDescriptor->properties.dataType;
    }

}


void processConstValueNode (AST_NODE *constValueNode) {
    if(constValueNode->semantic_value.const1->const_type == INTEGERC)
        constValueNode->dataType = INT_TYPE;
    else
        constValueNode->dataType = FLOAT_TYPE;
}


void checkReturnStmt (AST_NODE *returnNode) {
    AST_NODE *parent = returnNode->parent;
    AST_NODE *type;
    AST_NODE *expr = returnNode->child;

    while (parent != NULL) {
        if (parent->nodeType == DECLARATION_NODE)
            if (parent->semantic_value.declSemanticValue.kind == FUNCTION_DECL)
                break;
        parent = parent->parent;
    }
    type = parent->child;
    processExprNode(expr);

    if (type->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.dataType != expr->dataType) {
        // incompatible return type
        printErrorMsg(returnNode, RETURN_TYPE_UNMATCH);
    }
}


// xatier
// block
//     : decl_list stmt_list
//     | stmt_list
//     | decl_list
void processBlockNode (AST_NODE *blockNode) {
    AST_NODE *child = blockNode->child;
    printf("processing node: %p\n", child);
    exit(1);
    openScope();
    while (child) {
        switch (child->nodeType) {
            case VARIABLE_DECL_LIST_NODE:
                processDeclarationNode(child);
                break;
            case STMT_LIST_NODE:
                processStmtNode(child);
                break;
            default:
                break;
        }

        child = child->rightSibling;
    }
    closeScope();
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
    AST_NODE* dim = idNode->child;
    DATA_TYPE dimType;
    int count = 0;

    while(dim != NULL) {
        if (count == 0 && ignoreFirstDimSize) {
            typeDescriptor->properties.arrayProperties.sizeInEachDimension[count] = 0;
        }
        else {
            if(dim->nodeType == EXPR_NODE) {
                processExprNode(dim);
                dimType = dim->dataType;
            }
            else if(dim->nodeType == IDENTIFIER_NODE) {
                processVariableRValue(dim);
                dimType = dim->dataType;
            }
            else if(dim->nodeType == STMT_NODE) {
                checkFunctionCall(dim);
                dimType = dim->child->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.functionSignature->returnType;
            }
            else {
                processConstValueNode(dim);
                dimType = dim->dataType;
            }

            if (dimType != INT_TYPE) {
                // array subscription is not an integer
                printErrorMsg(idNode, ARRAY_SUBSCRIPT_NOT_INT);
                typeDescriptor->properties.arrayProperties.dimension = 0;
                return;
            }
            //typeDescriptor->properties.arrayProperties.sizeInEachDimension[count] = dim->semantic_value.exprSemanticValue.constEvalValue.iValue;
            typeDescriptor->properties.arrayProperties.sizeInEachDimension[count] = 256; //no need to check bound
        }

        count++;
        dim = dim->rightSibling;
    }

    typeDescriptor->properties.arrayProperties.dimension = count;
}


// xatier
//
// function_decl
//     : type ID ( param_list ) { block }
//     | VOID ID ( param_list ) { block }
//     | type ID ( ) { block }
//     | VOID ID ( ) { block }
//     ;
void declareFunction (AST_NODE *declarationNode) {
    AST_NODE *id    = declarationNode->child->rightSibling;
    AST_NODE *param = declarationNode->child->rightSibling->rightSibling->child;
    AST_NODE *block = declarationNode->child->rightSibling->rightSibling->rightSibling;
    processTypeNode(declarationNode->child);
    SymbolTableEntry *nameCheck;
    SymbolAttribute *attribute;
    Parameter *current = NULL, *head = NULL;
    int count = 0;

    // XXX: debug
    printf("[+] function %s\n", id->semantic_value.identifierSemanticValue.identifierName);
    if (declarationNode->child->semantic_value.identifierSemanticValue.symbolTableEntry == NULL)
        return;

    nameCheck = retrieveSymbol(id->semantic_value.identifierSemanticValue.identifierName);

    // XXX: debug
    printf("[-] function %s\n", nameCheck);
    printf("[+] function %s\n", id->semantic_value.identifierSemanticValue.identifierName);

    while (nameCheck != NULL) {
        if (nameCheck->attribute->attributeKind == FUNCTION_SIGNATURE)
            break;
        nameCheck = nameCheck->sameNameInOuterLevel;
    }

    if (nameCheck != NULL) {
        // id redeclared
        printErrorMsgSpecial (id, id->semantic_value.identifierSemanticValue.identifierName, SYMBOL_REDECLARE);
        return;
    }

    attribute = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
    attribute->attributeKind = FUNCTION_SIGNATURE;

    while (param != NULL) {
        count++;
        processDeclarationNode(param);
        current = (Parameter*)malloc(sizeof(Parameter));
        if (head == NULL)
            head = current;
        current->parameterName = current->parameterName = param->child->rightSibling->semantic_value.identifierSemanticValue.identifierName;;
        current->next = NULL;
        current->type = declarationNode->child->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
        param = param->rightSibling;
        current = current->next;
    }

    attribute->attr.functionSignature->parametersCount = count;
    attribute->attr.functionSignature->parameterList = head;
    attribute->attr.functionSignature->returnType = declarationNode->child->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.dataType;

    id->semantic_value.identifierSemanticValue.symbolTableEntry = enterSymbol(id->semantic_value.identifierSemanticValue.identifierName, attribute);

    processBlockNode(block);
}
