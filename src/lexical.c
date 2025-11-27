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
#include <ctype.h>
#include <setjmp.h>
#include <stdlib.h>
#include "listspec.h"

#define LPARTOK EOF+1
#define RPARTOK EOF+2
#define QUOTTOK EOF+3
#define IDTOKEN EOF+4
#define NUMTOK  EOF+5
#define DQUOTTOK EOF+6


int nexttoken(FILE * infile, long * tokid, float *fval);
int gettoken (FILE * infile, long * tokid, float *fval);
int sexp(FILE * infile, SLC ** pp);
int lexp (FILE * infile, SLC **pp);
int q_sexp (FILE * infile, SLC **pp, int toktype);
int nextch (FILE * infile);

#define ungettoken() ungetflag = TRUE

static int nestlevel = 0; /* records bracket depth */


int nextch (FILE *infile)
{
int c,i;

/* gets the next character from the input stream */
/* looking for end of lines, which if it finds, */
/* then outputs the nesting level * prompts */

c = fgetc(infile);
if (c == '\n' && infile == stdin) {
	for (i=0; i<nestlevel; i++) printf("*");
} /* end test */
return c;
} /* end function nextch */



int nexttoken(FILE *infile, long *tokid, float *fval)
{
char astr[MAXIDLEN];
int atomlength, toktype;
char cc;
float locfval;

/* returns the next lexical token in the input stream */
/* returns EOF when file ends */

/* skip leading spaces and whitespace and comments */
do {
	cc = (char)nextch(infile);
	if (feof(infile)) return EOF;
	if (cc == ';') {
		while((cc = (char)nextch(infile)) != EOF && cc != '\n') ; /* do nothing */
	}
} while (isspace(cc));

if (feof(infile)) return EOF;

/* test for brackets and quote char */

if (cc == '(') return LPARTOK;
if (cc == ')') return RPARTOK;
if (cc == 0x27) return QUOTTOK; /*  ' quote character */
if (cc == 0x22) return DQUOTTOK; /* " quote character */

/* if not any of these must be id token */
/* keep looking for space or other special character */

atomlength = 0;
while (isgraph(cc)) {
	if (feof(infile)) return EOF;
	if (cc == '!') {
		cc = (char) nextch(infile);    /* escape character */
	} else 	if (cc == '(' || cc == ')' || cc == 0x27 || cc == ';') {
		break; /* special character found */
	}
	astr [atomlength++] = cc;
	if (atomlength == MAXIDLEN ) {
		puts("Error: identifier too long");
		longjmp (main_env, 2);
	}
	cc = (char) nextch(infile);
} /* end loop */

if (feof(infile)) return EOF;
if (isgraph(cc)) {
	ungetc (cc, infile);  /* unget the last character, ready for next token */
}

astr[atomlength] = 0; /* make it a c string */
locfval = atof(astr);

/* see if number or an atom */
if (locfval != 0.0 || isdigit(astr[0])) {
	toktype = NUMTOK;
	*fval = locfval;		
} else {
	/* assume identifier */
	*tokid = (long) putident (astr);
	toktype = IDTOKEN;
} /* end number or atom test */

return toktype;

} /* end nexttoken */




static int ungetflag = FALSE;

int gettoken (FILE *infile, long  *tokid, float *fval)
{
static long int lasttokid;
static int      lastanswer;
static float    lastfval;

if (ungetflag == FALSE) {
	lastanswer = nexttoken (infile, &lasttokid, &lastfval);
}
*tokid = lasttokid;
*fval = lastfval;
ungetflag = FALSE;
return lastanswer;

} /* end function gettoken */







int lex_sexp(FILE *infile, SLC **retptr)
{

/* used for the lexical analysis of atoms */
/* reads from the already open input file until a s expression read */
/* returns TRUE if valid s_exp read , return false if EOF on input found */

nestlevel = 0;
return sexp (infile, retptr);

} /* end function lex_sexp */




int sexp(FILE *infile, SLC **pp)
{
int toktype, ans;
long int numidval;
float fval;
SLC *wkptr;

toktype = gettoken(infile, &numidval, &fval);
switch (toktype) {
	case LPARTOK :
		nestlevel++;
		ans = lexp (infile, pp); /* cell will be assigned by lexp */
		nestlevel--;
		return ans;
	case RPARTOK :
		sprintf (outbuf, "Error: unexpected right bracket\n");
		condpr (stdout);
		ungetflag = FALSE;
		longjmp(main_env, 2); /* this forces a function exit */
	case QUOTTOK :
	case DQUOTTOK :
		return q_sexp (infile, pp, toktype );
	case IDTOKEN :
		*pp = wkptr = getfree();
		wkptr->lstat = IDATOM;
		wkptr->r.idval = (int) numidval;
		return TRUE;
	case NUMTOK  :
		*pp = wkptr = getfree();
		wkptr->lstat = NUMATOM;
		wkptr->r.rigval = fval;
		return TRUE;
	default:
		return ungetflag = FALSE; /* EOF case, and any errors */
} /* end case */

} /* end function sexp */




int lexp (FILE *infile, SLC **pp)
{
SLC *wkptr, *nptr, *n2ptr;
int tokid, ans;
long int numidval;
float fval;

/* left bracket already got */
*pp = wkptr = getfree(); /* first cell of list */
tokid = gettoken (infile, &numidval, &fval);
if (tokid == RPARTOK) {
	return TRUE; /* empty list */
}
if (tokid == EOF) {
	return ungetflag = FALSE;
}
/* must be a s_exp of some sort, so pass onto sexp procedure */
ungettoken();
mark_req (wkptr);
ans = sexp (infile, &nptr);
mark_not (wkptr);
if (ans == TRUE) {
	wkptr->r.rigptr = nptr;
	/* nptr already 1st ele  of list */
} else {
	return FALSE;
}
while (TRUE) {
	tokid = gettoken (infile, &numidval, &fval);
	if (tokid == RPARTOK) {
		return TRUE; /* end of list */
	}
	if (tokid == EOF) {
		printf("Error: unexpexted EOF\n");
		return ungetflag = FALSE;
	}
	/* must be a s_exp of some sort, so pass onto sexp procedure */
	ungettoken();
	mark_req (wkptr);
	ans = sexp (infile, &n2ptr);
	if (ans) {
		nptr->lefptr = n2ptr;
		nptr = n2ptr;	
		mark_not (wkptr);
	} 
} /* end loop */
} /* end function lexp */



int q_sexp (FILE *infile, SLC **pp, int toktype)
{
SLC *wkptr, *nptr, *n2ptr;
int ans;

/* to process a quoted s expression */
/* quote character already got, called from sexp */
/* sets up quoted list, then calls sexp or reads till next quotation mark*/

*pp = wkptr = getfree(); /* first cell of list */
mark_req(wkptr);

nptr = getfree(); /* the quote element of the list */
wkptr->r.rigptr = nptr;
nptr->lstat = IDATOM;
nptr->r.idval = QUOTEID;

/* now call sexp to get rest of quoted expression */
/* sexp will decide whether a list or not */
if (toktype == QUOTTOK )
	{
	ans = sexp (infile, &n2ptr);
	mark_not (wkptr);
	if (ans == TRUE) {
		nptr->lefptr = n2ptr;
	}
	return ans;
} else {
	/* " type here - read till matching " or eof */
	char cc, astr[MAXIDLEN];
	int atomlength = 0;

	n2ptr = getfree();
	mark_not (wkptr);
	nptr->lefptr = n2ptr;
	n2ptr->lstat = IDATOM;
	while (TRUE) {
	  cc = (char) nextch(infile);
	  if (cc == EOF || cc == 0x22)
	  {
		break;
	  }
	  astr [atomlength++] = cc;
	  if (atomlength == MAXIDLEN ) {
		printf ("Error: identifier too long\n");
		longjmp (main_env, 2);
	  }
	} /* end loop */
	astr[atomlength] = 0; /* make it a c string */
	/* must mark_req n2ptr here if change putident to trigger gc */
	n2ptr->r.idval = putident (astr);
	return (!feof(infile));
}

} /* end function q_sexp */



