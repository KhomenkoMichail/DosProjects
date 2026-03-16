#include "TXLib.h"
#include <assert.h>
#include <stdio.h>
#include <windows.h>
#include <stdbool.h>

#include "graphicFunctions.h"


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
                              button->sizeX, button->sizeY);

    txDrawText (button->upperLeftCornerX, button->upperLeftCornerY,
                button->upperLeftCornerX + button->sizeX,
                button->upperLeftCornerY + button->sizeY,
                button->text);
}

bool pointInButton (const button_t* button, double pointX, double pointY) {
    assert(button);

    return (button->upperLeftX <= pointX && pointX <= (button->upperLeftX + button->sizeX)) &&
           (button->upperLeftY <= pointY && pointY <= (button->upperLeftY + button->sizeY));
}

void drawButtons (const button_t buttonsArr[]) {
    assert(buttonsArr);

    for (int i = 0; buttonsArr[i].text; i++)
        drawButton(buttonsArr + i, false);
}

int selectButton (const button_t buttonsArr[]) {
    assert(buttonsArr);

    while (! txGetAsyncKeyState (VK_ESCAPE)) {
        double x = txMouseX(), y = txMouseY();
        int mouseIsPressed = txMouseButtons();

        Sleep (10);

        if (!mouseIsPressed)
            continue;

        for (int i = 0; buttons[i].text; i++)
            if (pointInButton(buttons + i, x, y))
                return buttons[i].id;


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
            txBitBlt(txDC(), 0, 0, 1260, 1260, screenShotsArr[curScreen], 0, 0);
        }

        curScreen = (curScreen + 1) % screenCounter;
    }
}

void createWindow (void) {
        txCreateWindow(1260, 1260);
}

buttonId_t showMenu (void) {

    HDC menuBackground = txLoadImage("screensAndSound/menu.bmp");
    if (!menuBackground) {
            printf("Failed to load: %s\n", "screensAndSound/menu.bmp");
            return 1;
    }

    txBitBlt(txDC(), 0, 0, 1260, 1260, menuBackground, 0, 0);

    button_t buttonsArr[NUM_OF_BUTTONS + 1] = {
        {goPatch,   "PATCH", TX_LIGHTGREEN, TX_GREEN, 500, 100, 300, 100},
        {skipPatch, "EXIT",  TX_LIGHTRED,   TX_RED,   700, 100, 300, 100},
        {} };

    while (! txGetAsyncKeyState (VK_ESCAPE)) {
        drawButtons (buttonsArr)

        buttonId_t id = selectButton(buttonsArr);

        if (id == goPatch)
            drawButton(buttonsArr, true);

        if (id == skipPatch)
            drawButton(buttonsArr + 1, true);

        return id;
    }

    return escape;
}
