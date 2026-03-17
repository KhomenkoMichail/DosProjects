#include "txLib.h"
#include <stdio.h>
#include <stdlib.h>

#include "../include/patchFunctions.h"
#include "../include/fileFunctions.h"
#include "../include/graphicFunctions.h"

int main (int argc, char* argv[]) {

    if (argc != 3) return printf("Usage: %s aim.com changes.txt\n", argv[0]), 1;

    aimFile_t aimFile = {};

    aimFile.name = argv[1];

    const char* patchFileName = argv[2];


    createWindow();
    buttonId_t userChoice = showMenu();


    if (userChoice == goPatch)
        goPatchFunc (&aimFile, patchFileName);

    return 0;
}
