#ifndef PATCH_FUNCTIONS_H
#define PATCH_FUNCTIONS_H

struct aimFile_t {
    char* name;
    char* bufferCopy;
    unsigned int size;
};

int changeAimFile (aimFile_t* aimFile, const char* patchFileName);

int goPatchFunc (aimFile_t* aimFile, const char* patchFileName);

#endif
