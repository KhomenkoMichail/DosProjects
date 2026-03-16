#ifndef GRAPHIC_FUNCTIONS_H
#define GRAPHIC_FUNCTIONS_H

const int NUM_OF_BUTTONS = 2;

enum buttonId_t {
    escape      = -1,
    goPatch     =  1,
    skipPatch   =  2
};

struct button_t {
    buttonId_t id;
    const char* text;

    COLORREF defaultColor;
    COLORREF pressedColor;

    int upperLeftCornerX;
    int sizeX;

    int upperLeftCornerY;
    int sizeY;
};

#endif
