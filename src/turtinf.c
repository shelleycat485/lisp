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


#include <setjmp.h>
#include <stdio.h>
#include <math.h>
#include "listspec.h"
#include "turtinf.h"
#include "turtle.h"

int is_a_num (SLC *form, int arr[], int allowed);
SLC * makecell (int value);
SLC *lx_eval (SLC *inptr);

SLC * makecell (int value)
{
SLC *wkptr;

wkptr = getfree();
wkptr->lstat = NUMATOM;
wkptr->r.rigval = (long)value;
return wkptr;

} /* end makecell */


int is_a_num (SLC *form, int arr[], int allowed)
{
int j = 0;
SLC *arg,*evalarg;

/* checks form is a number, or series of numbers, returns array of values */
/* and count of them, input pointing to form */
/* only allows integer numbers */

if (isnullcell(form->lefptr)== FALSE) {
	arg = form->lefptr;
	do {
		evalarg = lx_eval(arg);
		if (evalarg == NULL || evalarg->lstat != NUMATOM) {
			report_error ("turtle graphics", "argument not numeric", evalarg, TRUE);
			return 0;
		} 
		if (j >= allowed)
		{
			report_error ("turtle graphics", "too many args", evalarg, TRUE);
			return 0;
		}
		arr[j++] = roundf(evalarg->r.rigval);
		arg = arg->lefptr;
	} while (arg);
} /* end if */
if (j != allowed)
{
	report_error ("turtle graphics", "too few args", evalarg, FALSE);
	return 0;
}
return j;
} /* end function is_a_num */


SLC *lx_initturtle                  (SLC *form)
{
int vals[2]; 

/* params x and y size wanted */
is_a_num (form, vals, 2); 

 InitTurtle (vals);;
 return NULL;
} /* initturtle */

SLC *lx_home                        (SLC *form)
{
SLC *wkptr = getfree(), *wk2ptr = getfree(), *wk3ptr = getfree();
int *xandy;
if (wkptr && wk2ptr && wk3ptr) {
 	xandy = Home ();
	wkptr->lstat   = LSLST;
	wkptr->r.rigptr = wk2ptr;
	wk2ptr->lstat  = NUMATOM;
	wk2ptr->r.rigval = xandy[0];
	wk2ptr->lefptr    = wk3ptr;
	wk3ptr->lstat  = NUMATOM;
	wk3ptr->r.rigval = xandy[1];
	return wkptr; /* 2 ele list, screen x y */
} else {
 	return NULL;
}

} /* home */

 SLC *lx_pendown                     (SLC *form)
{
int vals[1];

is_a_num (form, vals, 1);
return (makecell (PenDown (vals[0])));

} /* pendown */

SLC *lx_setfill                     (SLC *form)
{
int vals[1];

is_a_num (form, vals, 1);
return (makecell (SetFill (vals[0])));

} /* setfill */

SLC *lx_pencolour                   (SLC *form)
{
int vals[3];  // R G B 0-255

is_a_num (form, vals, 3);
return (makecell (PenColor (vals)));

} /* pencolour */

SLC *lx_fillcolour                (SLC *form)
{
int vals[3]; // R G B 0-255

is_a_num (form, vals, 3);
return (makecell (FillColor (vals)));

} /* fillcolour */

SLC *lx_turn                        (SLC *form)
{
int vals[1];

is_a_num (form, vals, 1);
return (makecell (Turn (vals[0])));

} /* turn */

SLC *lx_turnto                      (SLC *form)
{
int vals[1];

is_a_num (form, vals, 1);
return (makecell (TurnTo (vals[0])));

} /* turnto */

SLC *lx_move                        (SLC *form)
{
int vals[1];

is_a_num (form, vals, 1);
return (makecell (Move ((double)vals[0])));

} /* move */

SLC *lx_moveto                      (SLC *form)
{
int vals[2];

is_a_num (form, vals, 2);
return (makecell (MoveTo ((double)vals[0], (double)vals[1])));

} /* moveto */

SLC *lx_circle                      (SLC *form)
{
int vals[1];

is_a_num (form, vals, 1);
return (makecell (Circle (vals[0])));

} /* circle */

SLC *lx_ellipse                     (SLC *form)
{
int vals[2];

is_a_num (form, vals, 2);
return (makecell (Ellipse (vals[0], vals[1])));

} /* ellipse */

SLC *lx_rectangle                   (SLC *form)
{
int vals[2];

is_a_num (form, vals, 2);
return (makecell (Rectangle (vals[0], vals[1])));

} /* rectangle */


SLC *lx_onscreen                    (SLC *form)
{
	return (makecell (OnScreen ()));

} /* onscreen */


SLC *lx_polygon                    (SLC *form)
{
int vals[2];

	is_a_num (form, vals, 2);
	return (makecell (Poly (vals[0], (double)vals[1])));

} /* polygon */



