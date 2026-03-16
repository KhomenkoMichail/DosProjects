#include "TxLib.h"
#include <stdio.h>
#include <assert.h>
#include <errno.h>

#include "../include/patchFunctions.h"
#include "../include/fileFunctions.h"

int changeAimFile (aimFile_t* aimFile, const char* patchFileName) {
    assert(aimFile);
    assert(patchFileName);

    FILE* patchFile = fopen (patchFileName, "r");
    if (!patchFile) {
        fprintf(stderr, "Error of opening file \"%s\"", patchFileName);
        perror("");
        return 1;
    }

    unsigned int newByte = 0;
    unsigned int offset = 0;

    while (1) {
        int result = fscanf(patchFile, "%x:", &offset);
        if (result == EOF) break;
        if (result != 1) {
            printf("Invalid offset format\n");
            fclose(patchFile);
            return 1;
        }

        result = fscanf(patchFile, "%x", &newByte);
        if (result == EOF) {
            printf("Unexpected end of file after offset %x\n", offset);
            fclose(patchFile);
            return 1;
        }
        if (result != 1) {
            printf("Invalid newByte format after offset %x\n", offset);
            fclose(patchFile);
            return 1;
        }

        offset -= 0x100;
        aimFile->bufferCopy[offset] = (char)newByte;
    }

    rewriteAimFile (aimFile);

    if (fclose (patchFile) != 0) {
        fprintf(stderr, "Error of closing file \"%s\"", patchFileName);
        perror("");
        return 1;
    }

    return 0;
}

