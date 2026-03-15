#ifndef PATCH_FUNCTIONS_H
#define PATCH_FUNCTIONS_H

const int NUM_OF_SCREENSHOTS = 7;
const int COMMAND_LENGTH = 128;

struct aimFile_t {
    char* name;
    char* bufferCopy;
    unsigned int size;
};

void createWindow (void);

int changeAimFile (aimFile_t* aimFile, const char* patchFileName);

void runGif (void);

//int rewriteAimFile (aimFile_t* aimFile);

#endif
