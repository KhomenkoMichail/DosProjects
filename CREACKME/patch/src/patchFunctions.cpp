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

void runGif(void) {
    HDC screenShotsArr[NUM_OF_SCREENSHOTS] = {};
    char fileName[100];
    int screenCounter = 0;

    for (int num = 0; num < NUM_OF_SCREENSHOTS; num++) {
        sprintf(fileName, "screensAndSound/screen%d.bmp", num);
        screenShotsArr[num] = txLoadImage(fileName);
        if (screenShotsArr[num] == NULL) {
            printf("Failed to load: %s\n", fileName);
            break;
        }
        screenCounter++;
    }

    if (screenCounter == 0) {
        printf("No frames loaded\n");
        return;
    }

    int curScreen = 0;
    for (int i = 0; i < 25; i++) {
        txSleep(150);

        if (screenShotsArr[curScreen] != NULL) {
            txBitBlt(txDC(), 0, 0, 1260, 1260, screenShotsArr[curScreen], 0, 0);
        }

        curScreen = (curScreen + 1) % screenCounter;
    }
}

void createWindow (void) {
        txCreateWindow(1260, 1260);
}

