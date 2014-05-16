#include "symbolTable.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
// This file is for reference only, you are not required to follow the implementation. //

int HASH(char * str) {
	int idx=0;
	while (*str){
		idx = idx << 1;
		idx+=*str;
		str++;
	}
	return (idx & (HASH_TABLE_SIZE-1));
}

SymbolTable symbolTable;

SymbolTableEntry* newSymbolTableEntry(int nestingLevel)
{
    SymbolTableEntry* symbolTableEntry = (SymbolTableEntry*)malloc(sizeof(SymbolTableEntry));
    symbolTableEntry->nextInHashChain = NULL;
    symbolTableEntry->prevInHashChain = NULL;
    symbolTableEntry->nextInSameLevel = NULL;
    symbolTableEntry->sameNameInOuterLevel = NULL;
    symbolTableEntry->attribute = NULL;
    symbolTableEntry->name = NULL;
    symbolTableEntry->nestingLevel = nestingLevel;
    return symbolTableEntry;
}

void removeFromHashTrain(int hashIndex, SymbolTableEntry* entry)
{
    //XXX: What the heck is this function meant for!?
}

void enterIntoHashTrain(int hashIndex, SymbolTableEntry* entry)
{
    SymbolTableEntry* current = symbolTable.hashTable[hashIndex];
    SymbolTableEntry* outerLevel = retrieveSymbol(entry->name);
    SymbolTableEntry* scope = symbolTable.scopeDisplay[entry->nestingLevel];
    while(scope->nextInSameLevel != NULL)
        scope = scope->nextInSameLevel;

    entry->sameNameInOuterLevel = outerLevel;
    scope->nextInSameLevel = entry;
    while(current != NULL) {
        if(current->nextInHashTrain == NULL) {
            entry->nextInHashTrain = current->nextInHashTrain;
            current->nextInHashTrain->prevInHashTrain = entry;
            current->nextInHashTrain = entry;
            entry->prevInHashTrain = current;
            break;
        }
        else if(current->nextInHashTrain->nestingLevel <= entry->nestingLevel) {
            entry->nextInHashTrain = current->nextInHashTrain;
            current->nextInHashTrain->prevInHashTrain = entry;
            current->nextInHashTrain = entry;
            entry->prevInHashTrain = current;
            break;
        }
        current = current->nextInHashTrain;
    }
}

void initializeSymbolTable()
{
    int i;
    symbolTable.currentLevel = 0;
    symbolTable.scopeDisplayElementCount = 256;
    symbolTable.scopeDisplay = (SymbolTableEntry**)malloc(sizeof(SymbolTableEntry*)*256);
    symbolTable.scopeDisplay[0] = newSymbolTableEntry(0);
    for(i = 0; i < HASH_TABLE_SIZE; i++)
        symbolTable.hashTable[i] = newSymbolTableEntry(0);
}

void symbolTableEnd()
{
    int i;
    closeScope();
    free(symbolTable.scopeDisplay);
    for(i = 0; i < HASH_TABLE_SIZE; i++)
        free(symbolTable.hashTable[i]);
}

SymbolTableEntry* retrieveSymbol(char* symbolName)
{
    int i, index = HASH(symbolName);
    SymbolTableEntry* current = symbolTable.hashTable[index];

    while(current != NULL) {
        if(strcmp(current->name, symbolName) == 0)
            return current;
        current = current->nextInHashTrain;
    }
    return NULL;
}

SymbolTableEntry* enterSymbol(char* symbolName, SymbolAttribute* attribute)
{
    SymbolTableEntry* newSymbol = newSymbolTableEntry(symbolTable.currentLevel);
    newSymbol->name = symbolName;
    newSymbol->attribute = attribute;
    return newSymbol;
}

//remove the symbol from the current scope
void removeSymbol(char* symbolName)
{
    SymbolTableEntry* current = retrieveSymbol(symbolName);
    if(current != NULL) {
        if(current->prevInHashChain != NULL)
            current->prevInHashChain->nextInHashChain = current->nextInHashChain;
        if(current->nextInHashChain != NULL)
            current->nextInHashChain->prevInHashChain = current->prevInHashChain;
    }
}

int declaredLocally(char* symbolName)
{
    SymbolTableEntry* current = symbolTable.scopeDisplay[symbolTable.currentLevel];
    while(current != NULL) {
        if(strcmp(current->name, symbolName) == 0)
            return 1;
        current = current->nextInSameLevel;
    }
    return 0;
}

void openScope()
{
    symbolTable.currentLevel++;
    if(symbolTable.currentLevel >= symbolTable.scopeDisplayElementCount) {
        symbolTable.scopeDisplayElementCount *= 2;
        symbolTable.scopeDisplay = (SymbolTableEntry**)realloc(symbolTable.scopeDisplay, sizeof(SymbolTableEntry*)*symbolTable.scopeDisplayElementCount);
    }
    symbolTable.scopeDisplay[symbolTable.currentLevel] = newSymbolTableEntry(0);
}

void closeScope()
{
    SymbolTableEntry* current = symbolTable.scopeDisplay[symbolTable.currentLevel];
    SymbolTableEntry* next = current->nextInSameLevel;
    free(current);
    while(next != NULL) {
        current = next;
        next = next->nextInSameLevel;
        if(current->prevInHashChain != NULL)
            current->prevInHashChain->nextInHashChain = current->nextInHashChain;
        if(current->nextInHashChain != NULL)
            current->nextInHashChain->prevInHashChain = current->prevInHashChain;
        free(current);
    }
    symbolTable.currentLevel--;
}
