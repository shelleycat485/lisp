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

/* number of atom ids allowed */
#define MAXNUMIDS 256000
/* character space for atoms */
#define MAXID  500000


/* max length of an identifier */
#define MAXIDLEN 500


/* main list number of cells */
#define MAXLELE 5720000
/* these macros used as the header status for each cell */

#define LSLST 0
#define NUMATOM 1
#define IDATOM  2

/* def for lexical analysis routine */
extern int lex_sexp(FILE * infile, SLC **retval);
void set_control_c (void);


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

#define NOEVAL 0
#define EVAL   1

#define QUOTEID 1
#define TRUEID  2
#define EVALID  3
#define LAMID   4


extern jmp_buf  main_env;
extern int  trace,looplevel, garb_announce;

/* any functions starting lx_ are lisp primitives, accessible directly */

extern void lx_prin(FILE *fptr, SLC *lptr, int spflag, int escflag);
SLC *report_error (char *function, char *message, SLC *listarg, int showarg);
extern char outbuf[];
void condpr(FILE *fptr);

/* defs for the ident store, used to store atom strings */

extern    int        putident(char *);
extern    int        srchident(char *);
extern    char       *getident (int);
extern    int        idcount; /* identifiers in idstore index */
extern    int        idstuse; /* characters in idstore */
extern    const int  maxprims; /* number of primitive operations */
/* defs for main list access routines */

extern SLC              *getfree(void);
extern void             copycell (SLC *src, SLC *dest);
extern void             initmainlist(void );
extern SLC  *frlptr, *oblptr, *binlptr, *prlptr;
int                    isnullcell  (SLC *inptr);
extern SLC              *sear_oblist(SLC * inatom);
extern void            mark_req (SLC *cell);
extern void            mark_not (SLC *cell);
extern void            garbage_coll(int totalwipe);
extern void            recmark (SLC *cell);
void check_keyboard(void);

