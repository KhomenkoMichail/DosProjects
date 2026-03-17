#include <stdio.h>
#include <assert.h>
#include <errno.h>

#include "../include/patchFunctions.h"
#include "../include/fileFunctions.h"


int copyFileContent(aimFile_t* aimFile) {
    assert(aimFile);

    FILE* file = fopen(aimFile->name, "rb");
    if (!file) {
        fprintf(stderr, "Error of opening file \"%s\"", aimFile->name);
        perror("");
        return 1;
    }

    aimFile->size = getSizeOfFile(file);

    char* fileCopyBuffer = (char*)calloc(aimFile->size, sizeof(char));
    if (!fileCopyBuffer) {
        fprintf(stderr, "Error calloc!\n");
        fclose(file);
        return 1;
    }

    fread(fileCopyBuffer, 1, aimFile->size, file);

    if (fclose(file) != 0) {
        fprintf(stderr, "Error of closing file \"%s\"", aimFile->name);
        perror("");
        free(fileCopyBuffer);
        return 1;
    }

    aimFile->bufferCopy = fileCopyBuffer;
    return 0;
}

long getSizeOfFile(FILE* file) {
    assert(file);

    if (fseek(file, 0, SEEK_END) != 0) {
        fprintf(stderr, "Error seeking file");
        perror("");
        fclose(file);
        return 1;
    }

    long size = ftell(file);

    if (size == -1) {
        fprintf(stderr, "Error telling file");
        perror("");
        fclose(file);
        return 1;
    }

    rewind(file);
    return size;
}

int rewriteAimFile(aimFile_t* aimFile) {
    assert(aimFile);

    FILE* file = fopen(aimFile->name, "wb");
    if (!file) {
        fprintf(stderr, "Error of opening file \"%s\"", aimFile->name);
        perror("");
        return 1;
    }

    fwrite(aimFile->bufferCopy, 1, aimFile->size, file);

    if (fclose(file) != 0) {
        fprintf(stderr, "Error of closing file \"%s\"", aimFile->name);
        perror("");
        return 1;
    }

    return 0;
}

unsigned long long getFileHash(const char* fileCopyBuffer) {
    assert(fileCopyBuffer);
    unsigned long long hash = 5381;

    for(ssize_t numOfElement = 0; fileCopyBuffer[numOfElement] != EOF; numOfElement++)
        hash = ((hash << 5) + hash) + (unsigned long long)(fileCopyBuffer[numOfElement]);

    return hash;
}
