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
            current->nextInHashChain->prevInHashChain = entry;
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
}


void symbolTableEnd (void) {
    int i;
    closeScope();
    free(symbolTable.scopeDisplay);
    for(i = 0; i < HASH_TABLE_SIZE; i++)
        free(symbolTable.hashTable[i]);
}


SymbolTableEntry *retrieveSymbol (char *symbolName) {
    int i, index = HASH(symbolName);
    SymbolTableEntry *current = symbolTable.hashTable[index];

    while (current != NULL) {
        if (strcmp(current->name, symbolName) == 0)
            return current;
        current = current->nextInHashChain;
    }
    return NULL;
}


SymbolTableEntry *enterSymbol (char *symbolName, SymbolAttribute *attribute) {
    SymbolTableEntry *newSymbol = newSymbolTableEntry(symbolTable.currentLevel);
    newSymbol->name = symbolName;
    newSymbol->attribute = attribute;
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
    SymbolTableEntry *current = symbolTable.scopeDisplay[symbolTable.currentLevel];
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
