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

/* file listspec.h */

typedef struct listcell {
	unsigned  lstat : 3;
	unsigned  isfptr : 1;
	unsigned  gcmark : 1;
	unsigned  gcflagged : 1;
	struct listcell *lefptr; 
	union {
		struct listcell *rigptr;
		float  rigval;
		int    idval;
		FILE   *rigfp;
	} r;
} SLC ;

#define SMALLSTORE 0

#if SMALLSTORE
/* number of atom ids allowed */
#define MAXATOMS 1000
/* character space for atoms */
#define MAXATOMCHARS  6000
/* main list number of cells */
#define MAXLELE 6000
#else
/* number of atom ids allowed */
#define MAXATOMS 256000
/* character space for atoms */
#define MAXATOMCHARS  500000
/* main list number of cells */
#define MAXLELE 5720000
#endif

/* max length of an identifier */
#define MAXIDLEN 500


/* these macros used as the header status for each cell */

#define LSLST 0
#define NUMATOM 1
#define IDATOM  2

/* def for lexical analysis routine */
extern int lex_sexp(FILE * infile, SLC **retval);


#define TRUE   1
#define FALSE  0

/* used to define prin mode */

#define SPACE 1
#define NOSPACE 0
#define ESC 1
#define NOESC 0

#define PLUS 0
#define DIFFERENCE 1
#define TIMES 2
#define DIVIDE 3
#define SQRT 4

#define NOEVAL 0
#define EVAL   1

#define QUOTEID 1
#define TRUEID  2
#define EVALID  3
#define LAMID   4


extern jmp_buf  main_env;
extern int  trace,looplevel, garb_announce, syslogyes;

/* any functions starting lx_ are lisp primitives, accessible directly */

extern void lx_prin(FILE *fptr, SLC *lptr, int spflag, int escflag);
SLC *report_error (char *function, char *message, SLC *listarg, int showarg);
extern char outbuf[];
void condpr(FILE *fptr);

/* defs for the ident store, used to store atom strings */

extern    int        putident(char *);
extern    int        srchident(char *);
extern    char       *getident (int);
extern    int        atomidcount; /* identifiers in idstore index */
extern    int        atomcharsused; /* characters in idstore */
extern    const int  maxprims; /* number of primitive operations */
/* defs for main list access routines */

extern void             initmainlist(void );
extern SLC  *frlptr, *oblptr, *binlptr, *prlptr;
extern SLC  *binl_floor; /* current frame's binding-list floor for sear_oblist pass 1 */
int                    isnullcell  (SLC *inptr);
extern SLC              *sear_oblist(SLC * inatom, int usefloor);
extern void            garbage_coll(int totalwipe);
extern void            recmark (SLC *cell);
void check_keyboard(void);

/* getfree/copycell/mark_req/mark_not defined here (not liststor.c) as
   static inline so calls from every translation unit -- not just
   liststor.c's own -- can actually be inlined by the compiler. */

static inline void mark_req (SLC *cell)
{
/* sets the gcflagged bit in the cell, so any garbage collection */
/* will retain the cell */
if (cell) cell->gcflagged = 1;
} /* end function mark_req */

static inline void mark_not (SLC *cell)
{
/* clears the gcflagged bit in the cell, so any garbage collection */
/* will return the cell to the free list */
if (cell) cell->gcflagged = 0;
} /* end function mark_not */

static inline SLC *getfree(void)
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

static inline void copycell (SLC *src,SLC *dest)
{
/* copies the contents of the source to the destination  */
/* the garbage collection bits are not explicitly copied */
if (src) {
	dest->lstat = src->lstat;
	dest->isfptr = src->isfptr;
	dest->lefptr = src->lefptr;
	dest->r = src->r;
} else {
	dest->lstat = LSLST;
	dest->r.rigptr = NULL;
	dest->lefptr = 0;
}
} /* end function copycell */

