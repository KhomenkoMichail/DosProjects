#ifndef FILE_FUCTIONS_H
#define FILE_FUCTIONS_H

int copyFileContent (aimFile_t* aimFile);

long getSizeOfFile (FILE* file);

int rewriteAimFile (aimFile_t* aimFile);

unsigned long long getFileHash(const char* fileCopyBuffer);

#endif
