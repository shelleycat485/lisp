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


#include <stdio.h>
#include <setjmp.h>
#include <stdlib.h>
#include <math.h>
#include "listspec.h"

#define OUTBUFSIZE 256
char outbuf[(OUTBUFSIZE+2)];



void condpr(FILE *fptr)
{
		fputs  (outbuf, fptr);
} /* end condpr */





void lx_prin(FILE *fptr,SLC *lptr, int spflag, int escflag)
{
char *ident, c;
int  ival;
float fval;

/* prints the argument supplied to the screen, adding brackets etc */
/* no carriage return after expression */
/* can also be used to write to a file */

if (isnullcell(lptr)) {
	snprintf(outbuf,OUTBUFSIZE, "()");
	condpr(fptr);
} else {
	do {
	    switch (lptr->lstat) {
		case NUMATOM :
			if (lptr->isfptr == 1) {
				fval = (long)lptr->r.rigfp;
			} else {
				fval = lptr->r.rigval;
			}
			ival = 10000.0 * fval -
				(10000.0 * roundf(fval));
			if (abs(ival) < 10) {
			if (spflag == SPACE){
				snprintf(outbuf,OUTBUFSIZE, " %ld ",lround(fval));
			} else {
				snprintf(outbuf,OUTBUFSIZE,"%ld",lround(fval));
			}
			} else {
			if (spflag == SPACE){
				snprintf(outbuf,OUTBUFSIZE," %.4f ",fval);
			} else {
				snprintf(outbuf,OUTBUFSIZE,"%.4f",fval);
			}
			}
			condpr(fptr);
			break;
		case IDATOM :
			ident = getident(lptr->r.idval);
			if (escflag == ESC) {
				if (spflag == SPACE)
				{
					snprintf(outbuf,OUTBUFSIZE," ");
					condpr(fptr);
				}
				while (*ident) {
					c = *ident;
					if (c == '!' || c == '(' || c == 0x20
					 || c == ')' || c == 0x27) {
						snprintf(outbuf,OUTBUFSIZE,"!%c",c);
						condpr(fptr);
					} else if (c == 0x0a) {
						snprintf(outbuf,OUTBUFSIZE,"!\n");
						condpr(fptr);
					} else {
						snprintf(outbuf,OUTBUFSIZE,"%c",c);
						condpr(fptr);
					}
					ident++;
				} /* end loop on chars */
				if (spflag == SPACE)
				{
					snprintf(outbuf,OUTBUFSIZE," ");
					condpr(fptr);
				}
			} else {
				if (spflag == SPACE) {
					snprintf(outbuf,OUTBUFSIZE," %s ",ident);
					condpr(fptr);
				} else {
					snprintf(outbuf,OUTBUFSIZE,"%s",ident);
					condpr(fptr);
				}
			} /* end escape test */
			break;
		case LSLST  :
			snprintf(outbuf,OUTBUFSIZE,"(");
			condpr(fptr);
			if (lptr->r.rigptr) {
				lx_prin(fptr,lptr->r.rigptr, spflag, escflag);
			}
			snprintf(outbuf,OUTBUFSIZE,")");
			condpr(fptr);
			break;
		default:
			snprintf(outbuf,OUTBUFSIZE,"Bad_cell\n");
			condpr(fptr);
	} /* end switch */
		lptr = lptr->lefptr;
	} while (lptr) ;/* end loop */
} /* end null test */
if (ferror(fptr)) {
	fprintf(stderr,"Error: write to file failed\n");
	longjmp( main_env, 2);
}
} /* end function lx_prin */




