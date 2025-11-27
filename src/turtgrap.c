/* LISP Interpreter */
/* Copyright (C) 1992, 2022-2025 Roger Haxby
*
*  This program is free software: you can redistribute it and/or modify
*   it under the terms of the GNU General Public License as published by
*   the Free Software Foundation, either version 3 of the License, or
*   (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*
*   You should have received a copy of the GNU General Public License
*   along with this program.  If not, see <https://www.gnu.org/licenses/>.
*
*   roger@haxby.eu */


#include <unistd.h>
#include <X11/Xlib.h>
#include "turtle.h"
#include <math.h>

/* default used if the init call is too small only */
#define WINDOW_SIZE 500.0
#define MIN_WINDOW_SIZE 500.0

Display *theDisplay;
Window  theWindow;
Screen *theScreen;
GC      theGC;

XWindowAttributes Xwa;

int currentX; /* cartesian x, y */
int currentY;
double currentDirection = 90.0; /* degrees */
static int penDown = 1;
static int filled_flag=0;

#define DEGREES_TO_RADIANS  (3.14159L/180.0)

int Mapy(int y) 
{
  int X11y = Xwa.height;
  return X11y - y; 

}


int   SetFill (int filltype)
{
 filled_flag = filltype;
 return filled_flag;
}

int   PenColor (int *colarr)
{
 /* colarr 3 elements R G B */
 unsigned long foreground = 0L;
 foreground = (colarr[0] << 16 )  | (colarr[1] << 8) | colarr[2];
 XSetForeground(theDisplay, theGC, foreground);
 return 0;
}

int   FillColor (int *colarr)
{
 /* colarr 3 elements R G B */
 unsigned long fillcol = 0L;
 fillcol = (colarr[0] << 16 )  | (colarr[1] << 8) | colarr[2];
 XSetBackground(theDisplay, theGC, fillcol);
 return 0;
}

int   Poly ( int nsides, int len)
{
 double relturn = 360.0/nsides;
 int i;
 int oldpen = PenDown(1);
 for (i = 0; i < nsides; i++) {
   Move ((double) len);
   Turn(relturn); 
 }
 PenDown(oldpen);  
 return 0;
}

static int xandy[2];

int*  Home()
{
    XGetWindowAttributes(theDisplay, theWindow, &Xwa);
    xandy[0] = Xwa.width;
    xandy[1] = Xwa.height;

    currentX = Xwa.width/2.0;
    currentY = Mapy(Xwa.height/2.0);
    return xandy;
}

int PenDown(int newvalue)
{
 /* a non-zero value counts as pen down */
    if (newvalue) {
		penDown = 1;
	} else {
    		penDown = 0;
	}
    return penDown;
}

int   Turn (double angle) /* relative turn, in degrees */
{
    int temp;
    currentDirection = currentDirection + (double)angle;
    temp = (int)currentDirection % 360; 
    return temp;
}


int   TurnTo (double angle) /* absolute turn, in degrees */
{
    currentDirection = (int)angle % 360;
    return (int)currentDirection;
}

int   MoveTo (double xpos, double ypos)
{
   int newX, newY;
    /* first work out the new endpoint */
   newX = xpos;
   newY = Mapy(ypos);
    /* if the pen is down, draw a line */
    if (penDown) XDrawLine(theDisplay, theWindow, theGC,
            currentX, Mapy(currentY), newX, newY);
    /* in either case, move the tortoise */
    currentX = xpos;
    currentY = ypos;
    return OnScreen();
}

int   Move (double magnitude)
{
   int  xinc, yinc;
    /* first work out the new endpoint */
    xinc =  sin(currentDirection*DEGREES_TO_RADIANS)*(double)magnitude;
    yinc = cos(currentDirection*DEGREES_TO_RADIANS)*(double)magnitude;
    /* if the pen is down, draw a line */
    if (penDown) XDrawLine(theDisplay, theWindow, theGC,
                           currentX, Mapy(currentY),
                           currentX+xinc, Mapy(currentY + yinc));
    /* in either case, move the tortoise */
    currentX += xinc;
    currentY += yinc;
    return OnScreen();
}


int   Rectangle (double width, double height)
{
  if (filled_flag) {
	    XFillRectangle (theDisplay, theWindow, theGC,
	      currentX - width/2 , Mapy(currentY) - height/2,
		 (unsigned int)width, (unsigned int)height);
  } else {
	    XDrawRectangle (theDisplay, theWindow, theGC,
	      currentX - width/2 , Mapy(currentY) - height/2,
		 (unsigned int)width, (unsigned int)height);
  }
	    return OnScreen();
}


int   Circle (double radius)
{
	
  if (filled_flag) {
    XFillArc(theDisplay, theWindow, theGC,
	currentX - radius/2, Mapy(currentY) - radius/2,
        (unsigned int)radius, (unsigned int)radius, 0, 64 * 360);
  } else {
    XDrawArc(theDisplay, theWindow, theGC,
	currentX - radius/2, Mapy(currentY) - radius/2,
        (unsigned int)radius, (unsigned int)radius, 0, 64 * 360);
  }
    return OnScreen();
}


int   Ellipse (double rada, double radb)
{
	if (filled_flag) {
	    XFillArc(theDisplay, theWindow, theGC,
		currentX - rada/2, Mapy(currentY) - radb/2,
		(unsigned int)rada, (unsigned int)radb, 0, 64 * 360);
	} else {
	    XDrawArc(theDisplay, theWindow, theGC,
		currentX - rada/2, Mapy(currentY) - radb/2,
		(unsigned int)rada, (unsigned int)radb, 0, 64 * 360);
	}
    return OnScreen();
}

int   OnScreen ()
{
int mappedy;

    XGetWindowAttributes(theDisplay, theWindow, &Xwa);
    mappedy=Mapy(currentY);
  if ((currentX > 0.0 && currentX < Xwa.width) 
	&& (mappedy > 0.0 && mappedy < Xwa.height)) {
	return 1;
  } else {
	return 0;
  }
}


void  InitTurtle (int vals[]) /* called once to set up screen at start */
{
    int x = WINDOW_SIZE;
    int y = WINDOW_SIZE;
    if (vals[0] > MIN_WINDOW_SIZE) {
	x = vals[0];
    }
    if (vals[1] > MIN_WINDOW_SIZE) {
	y = vals[1];
    }
    /* passed x and y as screen size */
    theDisplay = XOpenDisplay(NULL);
    XSynchronize(theDisplay, True);
    theScreen = DefaultScreenOfDisplay(theDisplay);
    theWindow = XCreateSimpleWindow(theDisplay, RootWindowOfScreen(theScreen),
                                    0, 0,
                                    x, y, 0,
                                    BlackPixelOfScreen(theScreen),
                                    WhitePixelOfScreen(theScreen));
    XStoreName(theDisplay, theWindow, "Lisp Turtle Graphics");
    theGC = XCreateGC(theDisplay, theWindow, 0L, NULL);
    XSetForeground(theDisplay, theGC, BlackPixelOfScreen(theScreen)); 
    XSetFillStyle(theDisplay, theGC, FillSolid);
    XMapWindow(theDisplay,theWindow);
    XLowerWindow(theDisplay, theWindow);
    XGetWindowAttributes(theDisplay, theWindow, &Xwa);
    Home();

}


