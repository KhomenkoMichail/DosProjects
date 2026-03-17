#ifndef GRAPHIC_FUNCTIONS_H
#define GRAPHIC_FUNCTIONS_H

const int NUM_OF_SCREENSHOTS = 7;

const int COMMAND_LENGTH = 128;

const int NUM_OF_BUTTONS = 2;

const int ROUNDING_RADIUS = 45;

enum buttonId_t {
    errorCode   =  0,
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

void drawButton (const button_t* button, bool isPressed);

bool pointInButton (const button_t* button, double pointX, double pointY);

void drawButtons (const button_t buttonsArr[]);

buttonId_t selectButton (const button_t buttonsArr[]);

void runGif (void);

void createWindow (void);

buttonId_t showMenu (void);

#endif
