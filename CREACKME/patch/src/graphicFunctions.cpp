#include "TXLib.h"
#include <assert.h>
#include <stdio.h>
#include <windows.h>
#include <stdbool.h>

#include "../include/graphicFunctions.h"


void drawButton (const button_t* button, bool isPressed) {
    assert(button);

    txSetColor (TX_WHITE);

    if(!isPressed)
        txSetFillColor (button->defaultColor);
    else
        txSetFillColor (button->pressedColor);

    Win32::RoundRect (txDC(), button->upperLeftCornerX, button->upperLeftCornerY,
                              button->upperLeftCornerX + button->sizeX,
                              button->upperLeftCornerY + button->sizeY,
                              ROUNDING_RADIUS, ROUNDING_RADIUS);

    txDrawText (button->upperLeftCornerX, button->upperLeftCornerY,
                button->upperLeftCornerX + button->sizeX,
                button->upperLeftCornerY + button->sizeY,
                button->text);
}

bool pointInButton (const button_t* button, double pointX, double pointY) {
    assert(button);

    return (button->upperLeftCornerX <= pointX && pointX <= (button->upperLeftCornerX + button->sizeX)) &&
           (button->upperLeftCornerY <= pointY && pointY <= (button->upperLeftCornerY + button->sizeY));
}

void drawButtons (const button_t buttonsArr[]) {
    assert(buttonsArr);

    for (int i = 0; buttonsArr[i].text; i++)
        drawButton(buttonsArr + i, false);
}

buttonId_t selectButton (const button_t buttonsArr[]) {
    assert(buttonsArr);

    while (! txGetAsyncKeyState (VK_ESCAPE)) {
        double x = txMouseX(), y = txMouseY();
        int mouseIsPressed = txMouseButtons();

        Sleep (10);

        if (!mouseIsPressed)
            continue;

        for (int i = 0; buttonsArr[i].text; i++)
            if (pointInButton(buttonsArr + i, x, y))
                return buttonsArr[i].id;


    }

    return escape;
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
            txBitBlt(txDC(), 0, 0, 1000, 750, screenShotsArr[curScreen], 0, 0);
        }

        curScreen = (curScreen + 1) % screenCounter;
    }
}

void createWindow (void) {
        txCreateWindow(1000, 750);
}

buttonId_t showMenu (void) {

    HDC menuBackground = txLoadImage("screensAndSound/menu.bmp");
    if (!menuBackground) {
            printf("Failed to load: %s\n", "screensAndSound/menu.bmp");
            return errorCode;
    }

    txBitBlt(txDC(), 0, 0, 1000, 750, menuBackground, 0, 0);

    button_t buttonsArr[NUM_OF_BUTTONS + 1] = {
        {goPatch,   "PATCH", TX_LIGHTBLUE,  TX_BLUE,  650, 250, 550, 100},
        {skipPatch, "EXIT",  TX_LIGHTRED,   TX_RED,   100, 250, 550, 100},
        {} };

    while (! txGetAsyncKeyState (VK_ESCAPE)) {
        drawButtons (buttonsArr);

        buttonId_t buttonId = selectButton(buttonsArr);

        if (buttonId == goPatch)
            drawButton(buttonsArr, true);

        if (buttonId == skipPatch)
            drawButton(buttonsArr + 1, true);

        return buttonId;
    }

    return escape;
}

