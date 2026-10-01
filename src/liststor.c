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
	unsigned int len;
	unsigned char isheap;
	signed char height;   /* AVL height of the subtree rooted here, leaf = 1 */
	int less;             /* slot of subtree of strings < this one, 0 = none */
	int more;             /* slot of subtree of strings > this one, 0 = none */
} SmallString;


int  string_garbage(void);
char *ss_getstring(SmallString *s);
void ss_free(SmallString *s);
void ss_store(SmallString *s, const char *src);
void initatomstore(void);
void grow_atomstore(void);

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
/* and every definition that has compiled code, which points into it */
/* (also after a total wipe: the code is kept) */
compex_gc_roots();
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

string_garbage();

} /* end function garbage_coll */




/* mark_req/mark_not/getfree/copycell are now defined in listspec.h as
   static inline, so they're visible for inlining from every .c file. */


/* small move-to-front cache in front of sear_oblist()'s oblist walk,
   the same scheme as v3.25's atomcache in front of srchident(). Holds
   the last few ids looked up in the oblist, most recent first, with the
   entry cell found, or NULL if the id is not in the oblist (so the
   redefinition check on a primitive such as car doesn't walk the whole
   oblist every time). The binding list is always searched first, as
   before, so dynamic scoping is unchanged.
   An entry cell stays valid for good: the oblist only grows (lx_set
   adds an entry only when none is found, and changes values in place)
   and its cells are always marked by the gc. Only a NULL result can go
   stale, so lx_set calls oblcache_invalidate() when it adds an entry.
   A program that rplaca's or rplacd's the list (obl) returns could get
   round the cache, but that would corrupt the oblist anyway. */

#define OBLCACHE_SIZE 31

typedef struct {
	int id;       /* the atom id looked up */
	SLC *entry;   /* its oblist entry cell, NULL if it has none */
} OblCacheEntry;

static OblCacheEntry oblcache[OBLCACHE_SIZE];
static int oblcache_used = 0;   /* number of occupied entries, 0..OBLCACHE_SIZE */

void oblcache_invalidate(void)
{
	oblcache_used = 0;
} /* end function oblcache_invalidate */

/* a fresh id appends while there's room, otherwise displaces the last
   (least recently used) entry; either way it ends up at index 0 */
static void oblcache_record(int id, SLC *entry)
{
int i;

if (oblcache_used < OBLCACHE_SIZE) oblcache_used++;
for (i = oblcache_used - 1; i > 0; i--) {
	oblcache[i] = oblcache[i-1];
}
oblcache[0].id = id;
oblcache[0].entry = entry;
} /* end function oblcache_record */

SLC *sear_oblist (SLC *inatom)
{
int inid, i;
#ifdef DEBUG
int guardleft;
#endif
SLC *wkptr, *oblidptr;
OblCacheEntry found;

/* searches the oblist for an entry matching the id of the atom supplied */
/* searches the binding list before the oblist */
/* if found, returns the entry cell for the definition  */
/* if not found, returns a null pointer */

/* if (inatom == NULL) return NULL; taken our because redundant 29/1/2026 RH*/

if ((isnullcell(inatom)==FALSE) && (inatom->lstat == IDATOM)) {
	inid = inatom->r.idval;
#ifdef DEBUG
	guardleft = MAXLELE - 20;
#endif
	/* first the binding list, always searched in full */
	for (wkptr = binlptr; wkptr; wkptr = wkptr->lefptr) {
		oblidptr = wkptr->r.rigptr;
		if (inid == oblidptr->r.idval) {
			return wkptr; /* found the id match */
		}
#ifdef DEBUG
		if (--guardleft == 0){
			puts("Lisp Error in sear_oblist");
			longjmp (main_env, 2);
		}
#endif
	}
	/* then the cache: a hit moves to the front. The entry is copied
	   first, since the shift overwrites oblcache[i] */
	for (i = 0; i < oblcache_used; i++) {
		if (oblcache[i].id == inid) {
			found = oblcache[i];
			for ( ; i > 0; i--) {
				oblcache[i] = oblcache[i-1];
			}
			oblcache[0] = found;
			return found.entry;
		}
	}
	/* then the oblist itself; the result, found or not, is cached */
	for (wkptr = oblptr; wkptr; wkptr = wkptr->lefptr) {
		oblidptr = wkptr->r.rigptr;
		if (inid == oblidptr->r.idval) {
			break; /* found the id match */
		}
#ifdef DEBUG
		if (--guardleft == 0){
			puts("Lisp Error in sear_oblist");
			longjmp (main_env, 2);
		}
#endif
	}
	oblcache_record(inid, wkptr);
	return wkptr;
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



/* atomstore/atomindex are grown on demand by grow_atomstore() (see
   below), starting from atomcap entries and doubling up to the hard
   ceiling MAXATOMS. A slot's index number never changes once assigned;
   only these two arrays' base addresses move, under realloc(), when
   they grow.

   Because of that, a char* returned by getident()/ss_getstring() is
   only valid until the next call that can reach putident() (which may
   realloc() and relocate this array) -- never hold one across such a
   call. Existing callers comply: lx_system/lx_open/lx_prin use the
   pointer synchronously, within the same statement or before any
   further evaluation; lx_implode/lx_explode never hold a live pointer
   into this array across their own putident() calls (lx_implode copies
   one character at a time into its own local buffer and calls
   putident() only once, after its loop finishes; lx_explode copies the
   whole identifier into a local buffer before the loop that calls
   putident() per character). */
SmallString *atomstore = NULL;
int *atomindex = NULL;
int atomcap = 500;   /* current allocated slot capacity; grows toward MAXATOMS */
/* primitive n's name. Must match the switch in lx_eval_internal (main.c)
   and ptable in fill_table (compex.c), number for number: add, remove or
   renumber a primitive in all three */
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
"let*",  /* 70 */
"compex", /* 71  testing compilations */

 "defined" /* 72 */
 };

const int maxprims = 72;
int atomidcount = 0;
int atomcharsused = 0;

/* The atom store doubles as a height-balanced (AVL) binary search tree
   keyed on the identifier string. SmallString.less / .more hold the
   atomstore slot number of the subtree of strings that compare lower /
   higher (0 = none; slot 0 is never used). Links are slot numbers, not
   pointers, so they stay valid when grow_atomstore() realloc()s the
   array. Rebalancing only re-links nodes -- no node ever moves within
   atomstore -- so atomindex[id] == id always holds and nothing in
   atomindex ever needs adjusting. (A variant that re-laid nodes out
   would need an id back-pointer in each node to fix atomindex up.) */

static int atomroot = 0;   /* slot of the tree root, 0 = empty */
static int atomnext = 1;   /* next never-used slot/id; set by initatomstore() */

static int node_height(int slot)
{
	return slot ? atomstore[slot].height : 0;
}

static void update_height(int slot)
{
int hl = node_height(atomstore[slot].less);
int hr = node_height(atomstore[slot].more);

	atomstore[slot].height = (signed char)((hl > hr ? hl : hr) + 1);
}

static int rotate_right(int slot)
{
int pivot = atomstore[slot].less;

	atomstore[slot].less = atomstore[pivot].more;
	atomstore[pivot].more = slot;
	update_height(slot);
	update_height(pivot);
	return pivot;
}

static int rotate_left(int slot)
{
int pivot = atomstore[slot].more;

	atomstore[slot].more = atomstore[pivot].less;
	atomstore[pivot].less = slot;
	update_height(slot);
	update_height(pivot);
	return pivot;
}

/* links the already-stored node newslot into the subtree rooted at root
   and returns the (possibly new) subtree root. The caller has already
   checked that the string is absent. Recursion depth is the AVL height,
   at most about 27 for MAXATOMS nodes. */
static int avl_insert(int root, int newslot)
{
int balance;

	if (root == 0) return newslot;
	if (strcmp(ss_getstring(&atomstore[newslot]),
	           ss_getstring(&atomstore[root])) < 0) {
		atomstore[root].less = avl_insert(atomstore[root].less, newslot);
	} else {
		atomstore[root].more = avl_insert(atomstore[root].more, newslot);
	}
	update_height(root);
	balance = node_height(atomstore[root].less) - node_height(atomstore[root].more);
	if (balance > 1) {
		int l = atomstore[root].less;
		if (node_height(atomstore[l].less) < node_height(atomstore[l].more)) {
			atomstore[root].less = rotate_left(l);   /* left-right case */
		}
		return rotate_right(root);
	}
	if (balance < -1) {
		int r = atomstore[root].more;
		if (node_height(atomstore[r].more) < node_height(atomstore[r].less)) {
			atomstore[root].more = rotate_right(r);  /* right-left case */
		}
		return rotate_left(root);
	}
	return root;
} /* end function avl_insert */

void initatomstore(void)
{
/* stores each primitive directly into its own slot n, so that primitive
   n keeps id n (lx_eval() switches on those numbers), bypassing
   putident() -- which would hand out the next free slot instead -- and
   then links it into the tree */
 int n;

 atomroot = 0;
 atomnext = maxprims + 1;
 atomstore = malloc(atomcap * sizeof(SmallString));
 atomindex = malloc(atomcap * sizeof(int));
 if (atomstore == NULL || atomindex == NULL) {
	puts("Fatal: No atom store allocate");
	exit(6);
 }
 memset(atomindex, 0, atomcap * sizeof(int));

 for (n = 1; n <= maxprims; n++) {
	ss_store(&atomstore[n], primindex[n]);
	atomindex[n] = n;
	atomidcount++;
	atomcharsused += strlen(primindex[n]);
	atomroot = avl_insert(atomroot, n);
 }
}

int srchident(char *string)
{
/* looks the string up in the atom tree. Returns its id -- which equals
   its atomstore slot, see the note above -- or 0 (invalid index) if the
   string is not present. Iterative: a lookup never recurses. */

int slot = atomroot;
int cmp;

	while (slot) {
		cmp = strcmp(string, ss_getstring(&atomstore[slot]));
		if (cmp == 0) return slot;
		slot = (cmp < 0) ? atomstore[slot].less : atomstore[slot].more;
	}
	return 0;
} /* end function srchident */





void grow_atomstore(void)
{
/* doubles atomcap, up to the hard ceiling MAXATOMS. Reallocs into
   temporaries first and only commits atomstore/atomindex/atomcap once
   both succeed -- growing one array but not the other would desync
   them and corrupt every subsequent lookup. The atom tree's less/more
   links are slot numbers, so the array moving needs no fix-up. */
int newcap;
SmallString *newstore;
int *newindex;

newcap = atomcap * 2;
if (newcap > MAXATOMS) newcap = MAXATOMS;

newstore = realloc(atomstore, newcap * sizeof(SmallString));
newindex = realloc(atomindex, newcap * sizeof(int));
if (newstore == NULL || newindex == NULL) {
	puts("Fatal: No atom store allocate");
	exit(6);
}
atomstore = newstore;
atomindex = newindex;
memset(atomindex + atomcap, 0, (newcap - atomcap) * sizeof(int));
atomcap = newcap;
} /* end function grow_atomstore */

int putident (char *string)
{
/* stores the string away in the atomstore and links it into the atom
   tree, returning its id (index into atomindex). A string that is
   already present returns its existing id. */

int id;

/* searches for the string already there */
if ((id = srchident(string)) != 0) return id;
/* ids/slots are handed out sequentially and never reused (string GC is
   disabled), so the store is full once the next slot is past the end.
   Growing is a cheap realloc, and needs no tree fix-up since the links
   are slot numbers. At the hard ceiling MAXATOMS there is nothing left
   to reclaim, so running out is fatal. */
if (atomnext >= atomcap) {
	if (atomcap < MAXATOMS) {
		grow_atomstore();
	} else {
		puts("Fatal: No more atom/string space");
		exit (3);
	}
}

id = atomnext++;
ss_store(&atomstore[id], string);
atomindex[id] = id;
atomidcount ++;
atomcharsused += strlen(string);
atomroot = avl_insert(atomroot, id);
return id;
} /* end function putident */



int string_garbage(void)
{
/* TRIAL: string storage garbage collection is disabled. Identifiers and
   their heap strings are never reclaimed, so the atom tree only grows
   and putident() hits its fatal ceiling once MAXATOMS ids exist. Returns
   the number of chars reclaimed, which is always 0. ss_free() is unused
   for now. */
	return 0;
} /* end function string_garbage */




char *getident(int index)
{
/* returns a string pointer to the id whose index is supplied */
if (index < 1 || index > atomcap - 1) {
	puts("Fatal: invalid id");
	exit(20);
}
return ss_getstring(&atomstore[atomindex[index]]);
} /* end function getident */

void ss_store(SmallString *s, const char *src) {
	size_t len = strlen(src);
	s->len = (unsigned int)len;
	s->height = 1;
	s->less = 0;
	s->more = 0;

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





