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
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <malloc.h>
#include <setjmp.h>
#include "listspec.h"

/* definitions and access routines for the main lists */
/* also the atom identifier store */


/* the free list head is accessed by frlptr  */
/* the oblist head is accessed by    oblptr  */
/* the binding list, for lambda paramters is accessed by binlptr */
/* the property list is accessed by prlptr */

/* initmainlist()   sets up the main list and assigns all cells to       */
/* the one continuous list                                               */

/* SLC *getfree()    returns a pointer to a new cell,        */
/*  detaching it from the free list                                      */

/* void copycell (SLC *src, SLC *dest)           */
/* copies the contents of the source to the destination                  */



SLC *frlptr, *oblptr, *binlptr, *prlptr;
SLC *mlist ;
static int targele = MAXLELE;


void initmainlist(void)
{
SLC *wkptr;
int ncells;

/* create free list containing all cells */
/* set up initial oblist and other pointers */
oblptr = binlptr = prlptr = frlptr = mlist = NULL;

while (mlist ==  NULL && targele > 0 )
  {
	mlist = (SLC *)calloc (targele, sizeof(SLC));
	if (mlist == NULL) {
		targele -= 500;
	}
} /* end while loop */
if (mlist == NULL)
{
	printf("Fatal: No list allocate\n");
	exit(5);
}
wkptr = mlist; /* set pointers valid */
for (ncells = 0;ncells < targele;ncells++) {
	wkptr->lstat = LSLST;
	wkptr->r.rigptr = NULL;
	wkptr->gcmark = 0;
	wkptr->gcflagged = 0;
	wkptr->lefptr = frlptr;
	frlptr = wkptr;
	wkptr++;
} /* end loop */
} /* end function initmainlist */



void recmark (SLC *cell)
{
int i = 0;
while (cell) {

	/* does a recursive marking of all cells from the current one */
	/* if cell already marked, stop immediately, since recmark must have */
	/* been here already */

	if (++i>targele) {
		puts ("Fatal: recmark too many cells");
		exit(3);
	}
	if (cell->gcmark) {
		break;
	}
	cell->gcmark = 1;
	if (cell->lstat == LSLST) {
		recmark (cell->r.rigptr);
	}
	cell = cell->lefptr;
} /* end while loop */
} /* end function recmark */


static int gcnum = 0;


void garbage_coll(int totalwipe)
{
int i,reclaimed;
SLC *current;

/* does the garbage collection */

/* totalwipe is set true when the gc is called from the user break routine */
/* if totalwipe is set then clears all gcflagged bits too */
/* totalwipe also clears the binding list, since any lambda functs are aborted */
/* totalwipe also clears the lexical analysis list, and also */
/* resets the free list pointer, binding list pointer and hptr, which */
/* avoids corrupted lists */

gcnum++; /* cycle round of this counter does not matter */
if (totalwipe) {
	frlptr = binlptr = NULL;
	/* if a total wipe, also clears all the gcflagged bits from all cells */
	for (i=0, current = mlist; i< targele ;i++, current++) {
	current->gcflagged = 0;
	}
}

mark_req (oblptr); /* the oblist */
mark_req (prlptr); /* the property list */
mark_req (binlptr); /* the binding argument list */
/* then marks all cells with the gcflagged bit set */
for (i=0, current = mlist; i< targele ;i++, current++) {
	if (current->gcflagged && (!current->gcmark)) {
		recmark(current);
	}
}
mark_not (oblptr);
mark_not (binlptr);
mark_not (prlptr);

/* does a pass returning all unmarked cells to the free list */
for (i= reclaimed = 0, current = mlist; i< targele ;i++, current++) {
	if (!current->gcmark) {
		current->lstat = LSLST;
		current->lefptr = frlptr;
		frlptr = current;
		reclaimed++;
	} else {
		current->gcmark = 0; /* clear the mark ready for next time */
	}
}
if (reclaimed == 0) {
	puts("\nWarning: cannot reclaim any cells, evaluation stopped");
	longjmp( main_env , 2);
}
if (garb_announce) {
	sprintf (outbuf, "Garbage collection %d, of %d cells, %d reclaimed\n",gcnum, targele,reclaimed);
	condpr (stdout);
} /* end if announced */
} /* end function garbage_coll */




void mark_req (SLC *cell)
{
/* sets the gcflagged bit in the cell, so any garbage collection */
/* will retain the cell */ 
if (cell) cell->gcflagged = 1;
} /* end function mark_req */




void mark_not (SLC *cell)
{
/* clears the gcflagged bit in the cell, so any garbage collection */
/* will return the cell to the free list */ 
if (cell) cell->gcflagged = 0;
} /* end function mark_req */




SLC *getfree(void)
{
/* returns the first cell from the free list */

SLC *wkptr;

if (!frlptr) {
	garbage_coll(FALSE); /* not calling for a total wipe */
}
wkptr = frlptr;
frlptr = frlptr->lefptr;
wkptr->lstat = LSLST;
wkptr->r.rigptr = NULL;
wkptr->gcmark = 0;
wkptr->gcflagged = 0;
wkptr->lefptr = 0;
wkptr->isfptr = 0;


return wkptr;
} /* end function getfree */




void copycell (SLC *src,SLC *dest)
{
/* copies the contents of the source to the destination  */
/* the garbage collection bits are not explicitly copied */
if (src) {
	dest->lstat = src->lstat;
	dest->isfptr = src->isfptr;
	dest->lefptr = src->lefptr;
	dest->r = src->r;
} else {
	dest = NULL; /* if blank cell null dest pointer too */
}
} /* end function copycell */





SLC *sear_oblist (SLC *inatom)
{
int inid, pass, guardcount;
SLC *wkptr, *oblidptr;

/* searches the oblist for an entry matching the id of the atom supplied */
/* searches the binding list before the oblist */
/* if found, returns the entry cell for the definition  */
/* if not found, returns a null pointer */

if (inatom == NULL) return NULL;

if (isnullcell(inatom)==FALSE) {
	inid = inatom->r.idval;
	pass = guardcount = 1;
	while (pass <= 2) {
		if (pass == 1) {
			wkptr = binlptr; /* first pass of outer loop */
		} else {
			wkptr = oblptr;  /* second pass of outer loop */
		}
		while (wkptr) {
			oblidptr = wkptr->r.rigptr;
			if (inid == oblidptr->r.idval) {
				/* found the id match */
				return wkptr;
			}
			wkptr = wkptr->lefptr;
			if (guardcount++ == (MAXLELE - 20) ){
				puts("Lisp Error in sear_oblist");
				longjmp (main_env, 2);
			}
		} /* end loop */
		pass++;
	} /* end outer loop */
} /* end if non null target given */
return NULL; /* not found it */
} /* end function sear_oblist */


int isnullcell(SLC *inptr)
{
/* returns TRUE if the input pointer is zero, or a pointer to a null cell */
if (inptr == NULL) return TRUE;
if ((inptr->lstat == LSLST) && (inptr->r.rigptr == NULL) && (inptr->lefptr == 0)) {
	return TRUE;
} else {
	return FALSE;
} 
} /* end function isnullcell */




char idstore[MAXID],*idstptr = idstore; /* idstptr points to top of char store */
char *idindex[MAXNUMIDS] =
 {
  "",
"quote",   /* also defined in macro in listspec.h and used in main.c */
"true",    /* also defined in macro in listspec.h and used in main.c */
"eval",    /* also defined in macro in listspec.h and used in main.c */
"lambda",  /* also defined in macro in listspec.h and used in main.c */
"cdr",
"car",
"cons",
"and",
"or",
"cond",

"list",  /* 11 */
"loop",
"while",
"until",
"set",
"setq",
"eof",
"ordinal",
"minusp",
"system",

"plus",  /* 21 */
"times",
"difference",
"divide",
"listp",
"numberp",
"atom",
"null",
"not",
"length",

"obl",  /* 31 */
"print",  
"prin",
"princ",
"load",
"readch",
"explode",
"append",
"read",
"open",

"close",  /* 41 */
"put",
"remprop",
"get",
"implode",
"rplaca",
"rplacd",
"writec",
"writen",
"write",

"reverse",  /* 51 */
"eq",
"initturtle",
"home", 
"pendown",
"setfill",
"pencolour",
"fillcolour",
"turn",
"turnto",

"move",  /* 61 */
"moveto",
"circle",
"ellipse", 
"rectangle",
"onscreen",
"polygon",
"let"  /* 68 */
 };

const int maxprims = 68; 
int idcount =  68;
static int last_index_used = 68; /* to optimise the string storage */

int idstuse = 0;


int  string_garbage(void);

void initidstore(void)
{
 ;
}

int srchident(char *string)
{
/* looks down the idstore for an already existing ident */
/* returns 0 (invalid index) if none found        */

int srchindex;
int c1 = *string;

/*c1 = tolower (*string);*/
for (srchindex = 1; srchindex < MAXNUMIDS ; srchindex++ )
{
	if ( idindex[srchindex] 
           && c1 == /* tolower*/ (*idindex[srchindex])
           && strcmp(string,idindex[srchindex]) == 0 ) /* was strcasecmp */
	{
		return srchindex; /* found it */
	}
} /* end loop */
return 0;
} /* end function srchident */





int putident (char *string)
{
/* stores the string away in the ident store, returning the index to it */
/* accesses global data idstore, idstptr, idindex and idcount */

int res,length;

/* searches for the string already there */
if ((res = srchident(string)) != 0) return res;

/* check for space still in the store */
length = strlen(string);
if (idstuse+length >= MAXID-1 || idcount == MAXNUMIDS - 1) {
	if (string_garbage() < length )
	{
	    puts("Fatal: No more string space");
	    exit (3);
	}
}
/* look for somewhere to put string */
res = (last_index_used == MAXNUMIDS - 1) ? maxprims : last_index_used + 1;
while (res != last_index_used)
	{
	   if (idindex[res] == NULL )
	     {
			idcount ++;
			idindex[res] = strcpy(idstptr,string); /* copy string in */
			idstuse += length+1;
			idstptr += length+1; /* allow for terminating 0 */
			return last_index_used = res;
	     } /* end if string ok to store */
	     if (++res == MAXNUMIDS)
	      {
			res = maxprims;
	      }
	} /* end loop */
puts("Fatal: Too many identifiers");
exit (3);
} /* end function putident */



int string_garbage(void)
{
/* does a string storage garbage collection */
int i, charsreclaimed = 0, idsreclaimed = 0;
SLC *current;
char *strptr;

/* loop through the main list, finding all id pointers */
/* when found, set the msb of the character in the string store */
for (i=0, current = mlist ; i< targele ; i++ , current++) {
       if (current->lstat == IDATOM && current->r.idval > maxprims) {
	       strptr = idindex[current->r.idval];
	       *strptr |= 0x80;
       }
 }

/* go down the idindex, setting the pointer to NULL if the string */
/* can be erased, otherwise moving the pointer to where the string */
/* is going to be, moving string down the store, also clear the top bit */

for(i = maxprims+1 ; i < MAXNUMIDS; i++ )
    {
    if (idindex[i])
       {
	       if (*idindex[i] & 0x80)
	       {
			strptr = idindex[i];
			*strptr &= 0x7f; /* clear top bit */
			if (charsreclaimed)
			{
				idindex[i] -= charsreclaimed;
				strcpy (idindex[i] , strptr);
			}  /* end if charsreclaimed */
	       } else {
			charsreclaimed += strlen( idindex[i] );
			idindex[i] = NULL;
			idsreclaimed++;
	      }  /* end if top bit not marked */
       } /* end if */
} /* end for loop */

idstptr -=  charsreclaimed;
idstuse -= charsreclaimed;
idcount -= idsreclaimed;
if (garb_announce) {
	sprintf (outbuf, " Strings reclaimed, %d chars, %d ids\n",charsreclaimed, idsreclaimed);
	condpr (stdout);
} /* end if announced */
return charsreclaimed;
} /* end function string_garbage */




char *getident(int index)
{
/* returns a string pointer to the id whose index is supplied */
if (index < 1 || index > MAXID - 1) {
	puts("Fatal: invalid id");
	exit(20);
}
return (char *)(idindex[index]);
} /* end function getident */

