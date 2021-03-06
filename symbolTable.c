#include "symbolTable.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
// This file is for reference only, you are not required to follow the implementation. //

int HASH(char *str) {
	int idx = 0;
	while (*str) {
		idx = idx << 1;
		idx += *str;
		str++;
	}
	return (idx & (HASH_TABLE_SIZE-1));
}


SymbolTable symbolTable;


SymbolTableEntry *newSymbolTableEntry (int nestingLevel) {
    SymbolTableEntry* symbolTableEntry = (SymbolTableEntry *)malloc(sizeof(SymbolTableEntry));
    symbolTableEntry->nextInHashChain = NULL;
    symbolTableEntry->prevInHashChain = NULL;
    symbolTableEntry->nextInSameLevel = NULL;
    symbolTableEntry->sameNameInOuterLevel = NULL;
    symbolTableEntry->attribute = NULL;
    symbolTableEntry->name = NULL;
    symbolTableEntry->nestingLevel = nestingLevel;
    return symbolTableEntry;
}


void removeFromHashChain (int hashIndex, SymbolTableEntry *entry) {
    //XXX: What the heck is this function meant for!?
    // xatier: maybe remove entries that won't be used anymore?
    //         free some resource, be good to our planet.
}


void enterIntoHashChain (int hashIndex, SymbolTableEntry *entry) {
    SymbolTableEntry *current = symbolTable.hashTable[hashIndex];
    SymbolTableEntry *outerLevel = retrieveSymbol(entry->name);
    SymbolTableEntry *scope = symbolTable.scopeDisplay[entry->nestingLevel];
    while (scope->nextInSameLevel != NULL)
        scope = scope->nextInSameLevel;

    entry->sameNameInOuterLevel = outerLevel;
    scope->nextInSameLevel = entry;
    while (current != NULL) {
        if (current->nextInHashChain == NULL) {
            entry->nextInHashChain = current->nextInHashChain;
            current->nextInHashChain = entry;
            entry->prevInHashChain = current;
            break;
        }
        else if (current->nextInHashChain->nestingLevel <= entry->nestingLevel) {
            entry->nextInHashChain = current->nextInHashChain;
            current->nextInHashChain->prevInHashChain = entry;
            current->nextInHashChain = entry;
            entry->prevInHashChain = current;
            break;
        }
        current = current->nextInHashChain;
    }
}


void initializeSymbolTable (void) {
    int i;
    symbolTable.currentLevel = 0;
    symbolTable.scopeDisplayElementCount = 256;
    symbolTable.scopeDisplay = (SymbolTableEntry **)malloc(sizeof(SymbolTableEntry *) * 256);
    symbolTable.scopeDisplay[0] = newSymbolTableEntry(0);
    for(i = 0; i < HASH_TABLE_SIZE; i++)
        symbolTable.hashTable[i] = newSymbolTableEntry(0);

    SymbolAttribute *int1, *float1, *void1, *read, *fread, *write;
    Parameter *w;
    int1 = (SymbolAttribute *)malloc(sizeof(SymbolAttribute));
    float1 = (SymbolAttribute *)malloc(sizeof(SymbolAttribute));
    void1 = (SymbolAttribute *)malloc(sizeof(SymbolAttribute));
    read = (SymbolAttribute *)malloc(sizeof(SymbolAttribute));
    fread = (SymbolAttribute *)malloc(sizeof(SymbolAttribute));
    write = (SymbolAttribute *)malloc(sizeof(SymbolAttribute));
    int1->attributeKind = TYPE_ATTRIBUTE;
    float1->attributeKind = TYPE_ATTRIBUTE;
    void1->attributeKind = TYPE_ATTRIBUTE;
    read->attributeKind = FUNCTION_SIGNATURE;
    fread->attributeKind = FUNCTION_SIGNATURE;
    write->attributeKind = FUNCTION_SIGNATURE;
    int1->attr.typeDescriptor = (TypeDescriptor *)malloc(sizeof(TypeDescriptor));
    float1->attr.typeDescriptor = (TypeDescriptor *)malloc(sizeof(TypeDescriptor));
    void1->attr.typeDescriptor = (TypeDescriptor *)malloc(sizeof(TypeDescriptor));
    read->attr.functionSignature = (FunctionSignature *)malloc(sizeof(FunctionSignature));
    fread->attr.functionSignature = (FunctionSignature *)malloc(sizeof(FunctionSignature));
    write->attr.functionSignature = (FunctionSignature *)malloc(sizeof(FunctionSignature));
    w = (Parameter *)malloc(sizeof(Parameter));
    w->type = (TypeDescriptor *)malloc(sizeof(TypeDescriptor));
    w->next = NULL;
    w->type->kind = SCALAR_TYPE_DESCRIPTOR;
    w->type->properties.dataType = VOID_TYPE;
    w->parameterName = "yooo";
    int1->attr.typeDescriptor->kind = SCALAR_TYPE_DESCRIPTOR;
    float1->attr.typeDescriptor->kind = SCALAR_TYPE_DESCRIPTOR;
    void1->attr.typeDescriptor->kind = SCALAR_TYPE_DESCRIPTOR;
    read->attr.functionSignature->parametersCount = 0;
    fread->attr.functionSignature->parametersCount = 0;
    write->attr.functionSignature->parametersCount = 1;
    read->attr.functionSignature->parameterList = NULL;
    fread->attr.functionSignature->parameterList = NULL;
    write->attr.functionSignature->parameterList = w;
    read->attr.functionSignature->returnType = INT_TYPE;
    fread->attr.functionSignature->returnType = FLOAT_TYPE;
    write->attr.functionSignature->returnType = VOID_TYPE;
    int1->attr.typeDescriptor->properties.dataType = INT_TYPE;
    float1->attr.typeDescriptor->properties.dataType = FLOAT_TYPE;
    void1->attr.typeDescriptor->properties.dataType = VOID_TYPE;

    enterSymbol(SYMBOL_TABLE_INT_NAME, int1);
    enterSymbol(SYMBOL_TABLE_FLOAT_NAME, float1);
    enterSymbol(SYMBOL_TABLE_VOID_NAME, void1);
    enterSymbol(SYMBOL_TABLE_SYS_LIB_READ, read);
    enterSymbol(SYMBOL_TABLE_SYS_LIB_FREAD, fread);
    enterSymbol(SYMBOL_TABLE_SYS_LIB_WRITE, write);
}


void symbolTableEnd (void) {
    int i;
    for(i = 0; i < HASH_TABLE_SIZE; i++)
        free(symbolTable.hashTable[i]);
}


SymbolTableEntry *retrieveSymbol (char *symbolName) {
    int i, index = HASH(symbolName);
    SymbolTableEntry *current = symbolTable.hashTable[index]->nextInHashChain;

    while (current != NULL) {
        if (strcmp(current->name, symbolName) == 0)
            return current;
        current = current->nextInHashChain;
    }
    return NULL;
}


SymbolTableEntry *enterSymbol (char *symbolName, SymbolAttribute *attribute) {
    SymbolTableEntry *newSymbol = newSymbolTableEntry(symbolTable.currentLevel);
    newSymbol->name = (char *)malloc(sizeof(char) * (strlen(symbolName)+1));
    strncpy(newSymbol->name, symbolName, strlen(symbolName));
    newSymbol->attribute = attribute;
    enterIntoHashChain(HASH(newSymbol->name), newSymbol);
    return newSymbol;
}


//remove the symbol from the current scope
void removeSymbol (char *symbolName) {
    SymbolTableEntry *current = retrieveSymbol(symbolName);
    if (current != NULL) {
        if (current->prevInHashChain != NULL)
            current->prevInHashChain->nextInHashChain = current->nextInHashChain;
        if (current->nextInHashChain != NULL)
            current->nextInHashChain->prevInHashChain = current->prevInHashChain;
    }
}


int declaredLocally (char *symbolName) {
    SymbolTableEntry *current = symbolTable.scopeDisplay[symbolTable.currentLevel]->nextInSameLevel;
    while (current != NULL) {
        if (strcmp(current->name, symbolName) == 0)
            return 1;
        current = current->nextInSameLevel;
    }
    return 0;
}

void openScope (void) {
    symbolTable.currentLevel++;
    if (symbolTable.currentLevel >= symbolTable.scopeDisplayElementCount) {
        symbolTable.scopeDisplayElementCount *= 2;
        symbolTable.scopeDisplay = (SymbolTableEntry **)realloc(symbolTable.scopeDisplay, sizeof(SymbolTableEntry *) * symbolTable.scopeDisplayElementCount);
    }
    symbolTable.scopeDisplay[symbolTable.currentLevel] = newSymbolTableEntry(0);
}

void closeScope (void) {
    SymbolTableEntry *current = symbolTable.scopeDisplay[symbolTable.currentLevel];
    SymbolTableEntry *next = current->nextInSameLevel;
    free(current);
    while (next != NULL) {
        current = next;
        next = next->nextInSameLevel;
        if (current->prevInHashChain != NULL)
            current->prevInHashChain->nextInHashChain = current->nextInHashChain;
        if (current->nextInHashChain != NULL)
            current->nextInHashChain->prevInHashChain = current->prevInHashChain;
        free(current);
    }
    symbolTable.currentLevel--;
}
