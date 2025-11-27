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


/* turtle.h */
#ifndef turtle_inc
#define turtle_inc 1

void  InitTurtle (int[]); /* called once to set up screen, x y of screen */
int*  Home (); /* sets x and y to center of window, returns window size */
int   PenDown (int turnon);
int   SetFill (int filltype);
int   PenColor (int *colarr); // R G B values in array
int   FillColor (int *colour); // R G B values in array
int   Turn (double angle); /* relative turn, in degrees */
int   TurnTo (double angle); /* absolute turn, in degrees */
int   Move (double magnitude);
int   MoveTo (double xpos, double ypos);
int   Circle (double radius);
int   Ellipse (double rada, double radb);
int   Rectangle (double width, double height);
int   OnScreen ();
int   Poly ( int nsides, int len);

#endif


