/* LISP Interpreter */
/* Copyright (C) 1992, 2022-2026 Roger Haxby
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
#include <stdbool.h>
#include <malloc.h>
#include <syslog.h>
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

#define SSSIZE 15

typedef struct {
	union {
		char inline_buf[SSSIZE +1];
		struct {
			char *ptr;
			size_t capacity;
		} heap;
	} data;
	size_t len;
	unsigned char isheap;
} SmallString;


int  string_garbage(void);
char *ss_getstring(SmallString *s);
void ss_free(SmallString *s);
void ss_store(SmallString *s, const char *src);
void initatomstore(void);

SLC *frlptr, *oblptr, *binlptr, *prlptr;
SLC *binl_floor = NULL;
SLC *mlist ;
static int targele = MAXLELE;


void initmainlist(void)
{
SLC *wkptr;
int ncells;

/* create free list containing all cells */
/* set up initial oblist and other pointers */
oblptr = binlptr = prlptr = frlptr = mlist = NULL;
binl_floor = NULL;

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

initatomstore();
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
	binl_floor = NULL;
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
if (syslogyes) {
syslog (LOG_MAKEPRI (LOG_LOCAL1, LOG_NOTICE),
	"Cell gc: %d of %d cells %d reclaimed", gcnum, targele, reclaimed);
}
if (garb_announce) {
	sprintf (outbuf, "Garbage collection %d, of %d cells, %d reclaimed\n",gcnum, targele,reclaimed);
	condpr (stdout);
} /* end if announced */

string_garbage();

} /* end function garbage_coll */




/* mark_req/mark_not/getfree/copycell are now defined in listspec.h as
   static inline, so they're visible for inlining from every .c file. */


SLC *sear_oblist (SLC *inatom, int usefloor)
{
int inid, pass;
#ifdef DEBUG
int guardleft;
#endif
SLC *wkptr, *oblidptr, *stopat;

/* searches the oblist for an entry matching the id of the atom supplied */
/* searches the binding list before the oblist */
/* if found, returns the entry cell for the definition  */
/* if not found, returns a null pointer */

/* if (inatom == NULL) return NULL; taken our because redundant 29/1/2026 RH*/

if ((isnullcell(inatom)==FALSE) && (inatom->lstat == IDATOM)) {
	inid = inatom->r.idval;
	if (syslogyes) {
		syslog (LOG_MAKEPRI (LOG_LOCAL1, LOG_NOTICE),
			"start_search %d", inid);
	}	
	pass = 1;
#ifdef DEBUG
	guardleft = MAXLELE - 20;
#endif
	while (pass <= 2) {
		if (pass == 1) {
			wkptr = binlptr; /* first pass of outer loop */
			stopat = usefloor ? binl_floor : NULL;
		} else {
			wkptr = oblptr;  /* second pass of outer loop */
			stopat = NULL;
		}
		while (wkptr && wkptr != stopat) {
			oblidptr = wkptr->r.rigptr;
			if (inid == oblidptr->r.idval) {
				/* found the id match */
				if (syslogyes) {
					syslog (LOG_MAKEPRI (LOG_LOCAL1, LOG_NOTICE),
				    	"end_search %s", getident(inid));
				}
				return wkptr;
			}
			wkptr = wkptr->lefptr;
#ifdef DEBUG
			if (--guardleft == 0){
				puts("Lisp Error in sear_oblist");
				longjmp (main_env, 2);
			}
#endif
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



SmallString atomstore[MAXATOMCHARS];
int atomindex[MAXATOMS], *atomindptr = atomindex;
char idstore[MAXATOMCHARS],*idstptr = idstore; 
char *primindex[80] =
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

"+",  /* 21 */
"*",
"-",
"/",
"sqrt",
"listp",
"numberp",
"atom",
"null",
"not",

"length",  /* 31 */
"obl",
"print",
"prin",
"princ",
"load",
"readch",
"explode",
"append",
"read",

"open",  /* 41 */
"close",
"put",
"remprop",
"get",
"implode",
"rplaca",
"rplacd",
"writec",
"writen",

"write",  /* 51 */
"reverse",
"eq",
"initturtle",
"home",
"pendown",
"setfill",
"pencolour",
"fillcolour",
"turn",

"turnto",  /* 61 */
"move",
"moveto",
"circle",
"ellipse",
"rectangle",
"onscreen",
"polygon",
"let",  /* 69 */
"compex" /* 70  testing compilations */
 };

const int maxprims = 70;
int atomidcount = 0;
int atomcharsused = 0;

void initatomstore(void)
{
/* stores each primitive directly into its own slot n, bypassing
   putident()/srchident() -- srchident()'s primitive fast-path would
   otherwise "find" the name via primindex[] immediately and return
   before putident() ever reaches the ss_store() call that actually
   writes the string into atomstore[], leaving atomindex[n]==n
   correctly set but atomstore[n] permanently empty */
 int n;
 for (n = 1; n <= maxprims; n++) {
	ss_store(&atomstore[n], primindex[n]);
	atomindex[n] = n;
	atomidcount++;
	atomcharsused += strlen(primindex[n]);
 }
}

/* small move-to-front cache in front of srchident()'s linear scan.
   Holds the last few distinct atom slots found, most-recently-found
   entry first, so repeatedly-searched-for atoms (loop variables,
   function names, primitives -- nothing is excluded) get found in a
   handful of comparisons instead of a scan across the whole atomstore.
   Invalidated wholesale by string_garbage(), since a GC pass can
   reassign any slot number to a different string. */

#define ATOMCACHE_SIZE 15

typedef struct {
	int slot;   /* atomindex/atomstore slot this entry names */
} AtomCacheEntry;

static AtomCacheEntry atomcache[ATOMCACHE_SIZE];
static int atomcache_used = 0;   /* number of occupied entries, 0..ATOMCACHE_SIZE */

void atomcache_invalidate(void)
{
	atomcache_used = 0;
} /* end function atomcache_invalidate */

/* move-to-front: any match, first find or repeat, ends up at index 0.
   A fresh slot appends while there's room, otherwise displaces the
   last (least-recently-found) entry. Either way only the range
   between index 0 and the entry's landing point shifts -- no hit
   counts, no comparisons, no shuffling past empty entries. */
static void atomcache_record(int slot)
{
int i;

for (i = 0; i < atomcache_used; i++) {
	if (atomcache[i].slot == slot) break;
}
if (i == atomcache_used) {
	/* not cached yet: append if there's room, else land on (evict) the last entry */
	if (atomcache_used < ATOMCACHE_SIZE) atomcache_used++;
	i = atomcache_used - 1;
}
for ( ; i > 0; i--) {
	atomcache[i] = atomcache[i-1];
}
atomcache[0].slot = slot;
} /* end function atomcache_record */

int srchident(char *string)
{
/* looks down the atomstore for an already existing ident */
/* returns 0 (invalid index) if none found        */
/* assumes that atomstore[n] == 0 if empty store slot */

int srchindex, i;
const char *c1;

/* primitive names (1..maxprims) are registered once at startup and
   never reassigned (see initatomstore()/string_garbage()) -- for any
   n in that range, atomindex[n]==n and atomstore[n] holds
   primindex[n] permanently, so a direct compare against the fixed
   primindex[] table is always correct and far cheaper than falling
   through to the cache or the general scan. Confirmed via syslog
   instrumentation that this range is hit constantly in practice
   (211 times just loading lisplib/init.lsp; tens of thousands of
   times under a parse-heavy benchmark). */
for (srchindex = 1; srchindex <= maxprims; srchindex++) {
	if (strcmp(string, primindex[srchindex]) == 0) {
		return srchindex;
	}
}

/* check the small cache next */
for (i = 0; i < atomcache_used; i++) {
	int found = atomcache[i].slot;
	if (strcmp(string, ss_getstring(&atomstore[found])) == 0) {
		/* capture the match before recording -- atomcache_record()
		   reorders the table (move-to-front shift), so re-reading
		   atomcache[i] afterwards would return whatever ended up
		   at index i post-shift, not the entry we actually matched */
		atomcache_record(found);
		return found;
	}
}

/* general scan -- starts past maxprims since the primitive range was
   already handled above and can never match again */
/*c1 = tolower (*string);*/
for (srchindex = maxprims + 1; srchindex < MAXATOMS ; srchindex++ )
{
	if ( atomindex[srchindex])  {
		c1 = ss_getstring( &atomstore[atomindex[srchindex]]);
		if (*string == *c1 && strcmp(string,c1) == 0 ) { /* was strcasecmp */
			atomcache_record(srchindex);
			return srchindex; /* found it */
		}
	}
} /* end loop */
return 0;
} /* end function srchident */





int putident (char *string)
{
/* stores the string away in the atomstore, returning the index to it */

int res,srchindex;
/* searches for the string already there */
if ((res = srchident(string)) != 0) return res;
/* check for space still in slot numbers, before inserting */
if (atomidcount == MAXATOMS - 1) {
	/* string_garbage() returns the number of chars it reclaimed, so
	   0 means it found nothing to free -- genuinely out of room.
	   A non-zero return means it freed space, so it's fine to
	   continue below and reuse a slot it just cleared. */
	if (string_garbage() == 0)
	{
	    puts("Fatal: No more atom/string space");
	    exit (3);
	}
}

for (srchindex = 1; srchindex < MAXATOMS ; srchindex++ ){
	if (atomindex[srchindex] == 0) {
		ss_store(&atomstore[srchindex], string);
		atomindex[srchindex] = srchindex;
		atomidcount ++;
		atomcharsused += strlen(string);
		return srchindex;
	}
}

return 0;
} /* end function putident */



int string_garbage(void)
{
/* does a string storage garbage collection */
/* returns atomslots reclaimed */
int i, srchindex, a, idsreclaimed = 0;
int charsreclaimed = 0, heapcharsreclaimed = 0;
SLC *current;
bool flagarr[MAXATOMS];

/* any slot number a cache entry names could get reassigned to a
   different string by the reclaim pass below */
atomcache_invalidate();

for (i = 0; i < MAXATOMS; i++) {
	flagarr[i] = 0;
}
/* loop through the main list, finding all id pointers */
/* when found, indicate */
for (i=0, current = mlist ; i< targele ; i++ , current++) {
       if (current->lstat == IDATOM) {
	       flagarr[current->r.idval] = 1;
       }
 }

/* loop through atomstore looking for entries not flagged, */
/* they can be collected -- start past maxprims so primitive names */
/* (1..maxprims, registered once at startup and not necessarily */
/* referenced by any live cell at collection time) are never reclaimed */
for (srchindex = maxprims + 1; srchindex < MAXATOMS ; srchindex++ ){
	if (atomindex[srchindex] != 0 && flagarr[srchindex] == 0) {
		a = atomindex[srchindex];
		charsreclaimed += atomstore[a].len;
		if (atomstore[a].isheap) {
			heapcharsreclaimed += atomstore[a].len;
		}
		ss_free(&atomstore[a]);
		atomindex[a] = 0;
		idsreclaimed += 1;
	}
} /* end for loop */

atomidcount -= idsreclaimed;
atomcharsused -= charsreclaimed;
if (syslogyes) {
	syslog (LOG_MAKEPRI (LOG_LOCAL1, LOG_NOTICE),
	"String gc: %d chars, (heap %d), %d ids", charsreclaimed, heapcharsreclaimed, idsreclaimed);
}
if (garb_announce) {
	sprintf (outbuf, "String gc, %d chars, (heap %d), %d ids\n",charsreclaimed, heapcharsreclaimed, idsreclaimed);
	condpr (stdout);
} /* end if announced */
return charsreclaimed;
} /* end function string_garbage */




char *getident(int index)
{
/* returns a string pointer to the id whose index is supplied */
if (index < 1 || index > MAXATOMS - 1) {
	puts("Fatal: invalid id");
	exit(20);
}
return ss_getstring(&atomstore[atomindex[index]]);
} /* end function getident */

void ss_store(SmallString *s, const char *src) {
	size_t len = strlen(src);
	s->len = len;

	if (len < SSSIZE) {
		/* Fits inline */
		s->isheap = 0;
		memcpy(s->data.inline_buf, src, len+1);
	} else {
		s->isheap = 1;
		size_t cap = len+1;
		s->data.heap.ptr = malloc(cap);
		s->data.heap.capacity = cap;
		memcpy(s->data.heap.ptr, src, cap);
	}
}

char *ss_getstring(SmallString *s) {
	if (s->isheap) {
		return s->data.heap.ptr;
	} else {
		return s->data.inline_buf;
	}
}


void ss_free(SmallString *s) {
	if (s->isheap) {
		free(s->data.heap.ptr);
		s->data.heap.ptr = NULL;
	}
	s->len = 0;
	s->isheap = 0;
}





