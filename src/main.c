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
#include  <stdlib.h>
#include  <setjmp.h>
#include <sys/types.h>
#include <unistd.h>
#include <math.h>
#include "listspec.h"
#include "turtinf.h"
#include "linenoise.h"
#include <string.h>
#include <signal.h>
#include <sys/mman.h>
#include <errno.h>

#define LISPVER "3.64"

SLC *lx_eval_internal               (SLC *, bool);

void read_file (char * fname);
void Prompt_and_Read(int fout);  

int garb_announce = FALSE;
jmp_buf main_env;

/* shared between this process and the forked Prompt_and_Read child via
   mmap(MAP_SHARED), set up in main() before fork(). The child sets the
   flag when it sees Ctrl-C via the linenoise non-blocking API; this
   process's check_keyboard() polls, clears, and acts on it. */
static volatile sig_atomic_t *ctrlc_flag_ptr = NULL;

/* for reporting a failure while loading files: current_file is the file */
/* being read (innermost, for a load within a file), last_good_file the */
/* last file that was read all the way through */
#define SZ_LGF 80
static char current_file[SZ_LGF] = "";
static char last_good_file[SZ_LGF] = "";

void read_file (char *fname)
{
FILE *infilestream;
SLC *hptr; /* head of list to be evaluated */
char outer_file[SZ_LGF];

	/* an abort longjmps out of here, leaving current_file as the file */
	/* that failed; a normal return restores the outer file's name */
	snprintf(outer_file, SZ_LGF, "%s", current_file);
	snprintf(current_file, SZ_LGF, "%s", fname);
	if((infilestream = fopen(fname, "r")) == NULL){
		sprintf(outbuf, "Warning: Cannot find or open %s\n", fname);
		condpr (stdout);
	} else {
		/* read in the file */
		while (!feof(infilestream)) {
			hptr = NULL;
			if (lex_sexp(infilestream, &hptr)==TRUE ) {
				looplevel = 0;
				trace = FALSE;
				lx_eval(hptr);
			}
		} /* end while loop */
		fclose(infilestream);
		snprintf(last_good_file, SZ_LGF, "%s", fname);
	} /* end read file */
	snprintf(current_file, SZ_LGF, "%s", outer_file);
} /* end function read_file */


SLC *lx_load(SLC *filecell)
{
/* opens and reads in all of the specified file */

	if (filecell == NULL || filecell->lstat != IDATOM) {
		return report_error("load","must be given an identifier", filecell, TRUE);
	}
	read_file(getident(filecell->r.idval));	
	return NULL;
} /* end function lx_load */


#define LISP_LINE_BUF 4096

/* runs in a forked process */
void Prompt_and_Read(int fout){
	FILE *outStream = fdopen(fout, "w");
	struct linenoiseState l;
	char buf[LISP_LINE_BUF];
	char *line;

	/* send */
	while(1) {
		linenoiseEditStart(&l, -1, -1, buf, sizeof(buf), "Lisp:");
		while ((line = linenoiseEditFeed(&l)) == linenoiseEditMore)
			; /* blocks a byte at a time, same as linenoise()'s own internals */
		linenoiseEditStop(&l);

		if ( line == NULL) {
			if (errno == EAGAIN) {
				/* Ctrl-C: tell the parent, discard this line, re-prompt */
				if (ctrlc_flag_ptr) *ctrlc_flag_ptr = 1;
				continue;
			}
			break; /* Ctrl-D / EOF, as before */
		}
		linenoiseHistoryAdd(line); /* Add to the history */
		linenoiseHistorySave("history.txt"); /* Save history on disk */
		fputs(line, outStream);
		fputc('\n', outStream); /* Need n\ to drive lexical parser */
		fflush(outStream);
		free(line); /* needed because linnoise allocates */
	}
	fclose(outStream);
	exit(0);
} /* end function */


static FILE *inStream = NULL;

int main(int argc, char *argv[])
{
int jmpvalue,i;
volatile int fileguard = 0; /* volatile: changed after setjmp, must survive longjmp */
char *libname;
/*int fd[2];*/
int fd1[2];
int pipe1 = pipe(fd1);
SLC *hptr; /* head of list to be evaluated */

printf("LISP. Copyright R Haxby 1991-2026. Version ");
printf(LISPVER);
printf("\nThis program comes with ABSOLUTELY NO WARRANTY;\n");
printf("This is free software, and you are welcome to redistribute it\n");
printf("under certain conditions - see COPYING\n");

if (pipe1) {
	printf("Error: pipe failed in main.c\n");
}
inStream = fdopen(fd1[0], "r");
initmainlist();
linenoiseHistoryLoad("history.txt"); /* Load the history at startup */
linenoiseHistorySetMaxLen(50);

/* look for environment lisp library, and load it if found */
libname = getenv("LISPLIB");
if (libname != NULL) {
	printf ("...reading file %s\n", libname);
	read_file (libname);
}


/* look for series of load filenames */

jmpvalue = setjmp(main_env);   /* first def point for user break*/
if (jmpvalue != 0) {
	/* as at the second def point: drop the bindings of the aborted */
	/* evaluation and clear cells left gcflagged (e.g. pending let or */
	/* lambda bindings) before the files are read again */
	garbage_coll(TRUE);
	compex_abort_reset(); /* compiled code that was running is abandoned */
}

if (++fileguard > 10)
{
	if (last_good_file[0]) {
		printf("failed while reading file %s (last file read successfully: %s), bailing\n",
			current_file, last_good_file);
	} else {
		printf("failed while reading file %s (no file read successfully), bailing\n",
			current_file);
	}
	exit(7); // have read a lot of files in a loop, bailing
}

for(i=1; i < argc ; ++i){
	printf("...reading file %s\n", argv[i]);
	read_file (argv[i]);
} /* end i loop */

ctrlc_flag_ptr = mmap(NULL, sizeof(*ctrlc_flag_ptr), PROT_READ | PROT_WRITE,
                       MAP_SHARED | MAP_ANONYMOUS, -1, 0);
if (ctrlc_flag_ptr == MAP_FAILED) {
	printf("Error: mmap failed for ctrlc_flag_ptr in main.c\n");
	ctrlc_flag_ptr = NULL;
}
if (ctrlc_flag_ptr) *ctrlc_flag_ptr = 0;

    pid_t pid = fork();
    if (pid == -1) {
	printf("Error: failure to fork in main.c");
    }

    if (pid == 0){
	close(fd1[0]);
        Prompt_and_Read(fd1[1]);
    }

close(fd1[1]);

jmpvalue = setjmp(main_env);   /* second  def point for user break*/
sprintf(outbuf, "\n");
condpr (stdout);
if (jmpvalue != 0) {
	sprintf(outbuf, "Break In\n");
	condpr (stdout);
	garbage_coll(TRUE);
	compex_abort_reset(); /* compiled code that was running is abandoned */
} 
 /* now input from console */
while (!feof(inStream)) {
	hptr = NULL;
	if (lex_sexp(inStream, &hptr)==TRUE ) {
		looplevel = 0;
		trace = FALSE;
		sprintf(outbuf, "\n");
		condpr(stdout);
		lx_prin(stdout,lx_eval(hptr), SPACE, NOESC);
		sprintf(outbuf, "\n");
		condpr(stdout);
	}
} /* end while loop */

return 0;
} /* end function main */ 


SLC * report_error (char *function, char *message, SLC *listarg, int showarg)
{
/* general error reporting routine */
/* prints out the function name and error message given */
/* if showarg is TRUE then also prints out the list expression given */

SLC *retval = NULL;
sprintf( outbuf, "Function: %s Error: %s \n", function, message);
condpr (stdout);
if (showarg)
{
	lx_prin( stdout, listarg, SPACE, NOESC);
} /* end if */
while(TRUE)
	{
	int ccc;
	sprintf(outbuf, "\n Abort/Trace/Return");
	condpr(stdout);
	ccc = fgetc(inStream);
	switch (ccc)
		{
			case 'A' :
			case 'a' :
			case EOF :
				  longjmp(main_env,2);
			case 'T' :
			case 't' :
				sprintf(outbuf, "Abo t"); condpr(stdout);
				trace = TRUE;
				lex_sexp(inStream, &retval);
				return retval;
			case 'R' :
			case 'r' :
				sprintf(outbuf, "Abo r"); condpr(stdout);
				trace = FALSE;
				lex_sexp(inStream, &retval);
				lx_prin( stdout, retval, SPACE, NOESC);
				return retval;
			default:
			       break;  /* do nothing */
		} /* end switch */
	} /* end loop */
} /* end function report_error */




int trace; /* for switching on evaluation tracing */
int formname; /* for debug use, when getfree is called */

void check_keyboard(void)
{
/* polls the shared Ctrl-C flag set by the Prompt_and_Read child; if
   set, clears it and breaks out of the current evaluation */
	if (ctrlc_flag_ptr && *ctrlc_flag_ptr) {
		*ctrlc_flag_ptr = 0;
		longjmp(main_env, 1);
	}
} /* end function check_keyboard */


SLC *lx_eval (SLC *input)
{
	return lx_eval_internal(input, FALSE);
}

SLC *lx_eval_internal (SLC *inptr, bool isdefinedflag)
{
SLC *form, *newform, *res, *res1;
int redefs;
	check_keyboard();  /* allow user break in here */
	if (isnullcell(inptr) ) return NULL; /* test for null input here */
	formname = EVALID;
	res = form = NULL;
	mark_req (inptr);

	if (inptr->lstat == NUMATOM) {
		if (inptr->lefptr != NULL && inptr->isfptr == 0) {
			/* a literal number that is not the last argument is */
			/* still linked to the next one, e.g. the 1 in (or 1 2): */
			/* return a copy without the link, else the rest of the */
			/* arguments go with the value, (or 1 2) printing as 1 2. */
			/* File handles are not copied, close marks the cell itself */
			res = getfree();
			copycell(inptr, res);
			res->lefptr = NULL;
		} else {
			res = inptr;
		}
		goto endeval;
	}

	if (inptr->lstat == IDATOM) {
		res1 = sear_oblist (inptr);
		if (res1 != NULL) {
			if (isdefinedflag) {
				res = lx_true();
				goto endeval;
			}
			/* get the oblist entry value */
			res1 = res1->r.rigptr;
			res = res1->lefptr;
		} else if (inptr->r.idval > maxprims) {
			if (isdefinedflag) {
				res = NULL;
				goto endeval;
			} else {
			sprintf(outbuf, "Error: %s had no value here\n\r",getident(inptr->r.idval));
			condpr (stdout);
			trace = TRUE;
			}
		} else {
			/* is a primitive */
			if (isdefinedflag) {
				res = lx_true();
				goto endeval;
			}
			res = getfree();
			res->lstat = IDATOM;
			res->r.idval = putident("Subr");
		}
		goto endeval;
	} 

	form  = inptr->r.rigptr;
	mark_req(form);

	/*  try for redefinition - max two times */
	redefs = 0;
	res1 = sear_oblist(form);
	while (res1 && (redefs++ < 2)) {
		/* look at oblist result */
		res1 = res1->r.rigptr;
		res1 = res1->lefptr;
		/* redefine the form */
		mark_req(res1);
		copycell (res1,newform = getfree());
		mark_not(res1);
		newform->lefptr = form->lefptr;
		mark_not(form);
		form = newform;
		mark_req(form);
	res1 = sear_oblist(form);
	} /* redefinition loop */

	/* return null if null list contents */
	if (isnullcell(form)== TRUE) goto endeval;

	/* check for a lambda definition */
	if (form->lstat == LSLST &&
	form->r.rigptr != 0 &&
	(form->r.rigptr)->lstat == IDATOM &&
	(form->r.rigptr)->r.idval == LAMID ){
		/* found lambda def */
		formname = LAMID; /* for debug tracing only */
		/* compex mode 1 or 2 runs the compiled function, see compex.c */
		res = (compex_mode != 0) ? compex_lambda_call(inptr, form) : do_lambda (inptr, form);
		goto endeval;
	}


	/* check for "(3)" type of list */
	if (form->lstat != IDATOM) {
		report_error("eval","first element of a list must be a function", form, TRUE);
		goto endeval;
	}

	formname = form->r.idval;
	/* test for primitive names here */
	/* WARNING: this switch must match ptable in fill_table() in */
	/* compex.c, and primindex in liststor.c: the same primitive at */
	/* the same number, taking the same parameters, in the same order. */
	/* Adding, removing or renumbering a case, or changing how it */
	/* takes its parameters, needs the matching edit in fill_table(). */
	switch (formname) {
		case 1:	 
			res = form->lefptr; /* quote */
			break;
		case 2:
			res = lx_true();
			break;
		case 3:
			res1 = lx_eval(form->lefptr); /* eval */
			res = lx_eval(res1);
			break;
		case 4:
			report_error ("eval", "Lambda cannot be used directly, must be in a s-exp defining a function", form, TRUE);
			trace = TRUE;
			break;
		case 5:
			res = (lx_cdr (form));
			break;
		case 6:
			res = (lx_car (form));
			break;
		case 7:
			res = (lx_cons (form));
			break;
		case 8:
			res = (lx_and (form));
			break;
		case 9:
			res = (lx_or (form));
			break;
		case 10:
			res = (lx_cond (form));
			break;
		case 11:
			res = lx_list(form);
			break;
		case 12:
			res = lx_loop(form);
			break;
		case 13:
			res = lx_while(form, TRUE); /* while */
			break;
		case 14:
			res = lx_while(form, FALSE); /* until */
			break;
		case 15:
			res = (lx_set (form, EVAL));
			break;
		case 16:
			res = (lx_set (form, NOEVAL));
			break;
		case 17:
			res = (lx_eof (lx_eval(form->lefptr)));
			break;
		case 18:
			res = (lx_ordinal(lx_eval(form->lefptr)));
			break;
		case 19:
			res = (lx_minusp (lx_eval(form->lefptr)));
			break;
		case 20:
			lx_system(res = lx_eval(form->lefptr));
			break;
		case 21:
			res = (lx_plus (form, PLUS));
			break;
		case 22:
			res = (lx_plus (form, TIMES));
			break;
		case 23:
			res = (lx_plus (form, DIFFERENCE)); 
			break;
		case 24:
			res = (lx_plus (form, DIVIDE));
			break;
		case 25:
			res = (lx_plus (form, SQRT));
			break;
		case 26:
			res = (lx_listp (lx_eval(form->lefptr)));
			break;
		case 27:
			res = (lx_numberp (lx_eval(form->lefptr)));
			break;
		case 28:
			res = (lx_atom (lx_eval(form->lefptr)));
			break;
		case 29:
		case 30:	
			res = (lx_null (lx_eval(form->lefptr))); /* not  and null */
			break;
		case 31:
			res = lx_length (lx_eval(form->lefptr));
			break;
		case 32: 
			res = lx_obl(lx_eval(form->lefptr));
			break;
		case 33:
			lx_prin(stdout,res = lx_eval(form->lefptr), SPACE, NOESC);
			/* print operation - final cr */
			break;
		case 34:
			lx_prin(stdout,res = lx_eval(form->lefptr), NOSPACE, NOESC);
			break;
		case 35:
			/* princ - print with special chars escaped in */
			lx_prin(stdout,res = lx_eval(form->lefptr), NOSPACE, ESC);
			break;
		case 36:
			lx_load(res = lx_eval(form->lefptr));
			break;
		case 37:
			res = lx_readch(lx_eval(form->lefptr));
			break;
		case 38:
			res = (lx_explode (lx_eval(form->lefptr)));
			break;
		case 39:
			res = lx_append(form);
			break;
		case 40:
			res = lx_read(lx_eval(form->lefptr));
			break;
		case 41:
			res = lx_open(form);
			break;
		case 42:
			res = lx_close(lx_eval(form->lefptr));
			break;
		case 43:
			res = lx_put(form);
			break;
		case 44:
			res = lx_remprop(form);
			break;
		case 45:
			res = lx_get(form);
			break;
		case 46:
			res = (lx_implode (lx_eval(form->lefptr)));
			break;
		case 47:
			res = lx_rplaca(form);
			break;
		case 48:
			res = lx_rplacd(form);
			break;
		case 49:
			/* writec - write with special chars escaped in */
			res = lx_write(form, SPACE, ESC);
			break;
		case 50:
			/* writen - output like prin */
			res = lx_write(form, NOSPACE, NOESC);
			break;
		case 51:
			res = lx_write(form, SPACE, NOESC); /* write */
			break;
		case 52:
			res = lx_reverse (lx_eval(form->lefptr));
			break;
		case 53:
			res = lx_eq(form);
			break;
		case 54:
			res = lx_initturtle (form);
			break;
		case 55:
			res = lx_home (form);
			break;
		case 56:
			res = lx_pendown (form);
			break;
		case 57:
			res = lx_setfill (form);
			break;
		case 58:
			res = lx_pencolour (form);
			break;
		case 59:
			res = lx_fillcolour (form);
			break;
		case 60:
			res = lx_turn (form);
			break;
		case 61:
			res = lx_turnto (form);
			break;
		case 62:
			res = lx_move (form);
			break;
		case 63:
			res = lx_moveto (form);
			break;
		case 64:
			res = lx_circle (form);
			break;
		case 65:
			res = lx_ellipse (form);
			break;
		case 66:
			res = lx_rectangle (form);
			break;
		case 67:
			res = lx_onscreen (form);
			break;
		case 68:
			res = lx_polygon (form);
			break;
		case 69:
			res = lx_let (form);
			break;
		case 70:
			res = lx_letstar (form);
			break;
		case 71:
			res = lx_compex (form);
			break;
		case 72:
			res = lx_defined(form->lefptr);
			break;
		default:
			report_error ("eval", "non translatable list name", form, TRUE);
			trace = TRUE;
			break;
	} /* end switch on primitive names */
endeval:

	if (trace) {
		sprintf(outbuf, "\r\nEval trace: ");
		condpr (stdout);
		lx_prin(stdout,inptr, SPACE, NOESC);
		/*sprintf(outbuf, "\r\n");
		condpr (stdout); */
                if (trace++ >= 14) {
			longjmp( main_env, 2); /* getting fed up of trace printing */
		}
	} /* end tracing action */
	mark_not (inptr);
	mark_not (form); /* form was marked if a redefinition or lambda found */
	return res;
} /* end function eval */





int bind_uneval (SLC *formalargs, SLC *actualargs)
{
SLC *nextf, *tf, *temp;

/* makes actual formalarg the same as a list of the unevaluated actual arg */

	mark_req(nextf = getfree());
	mark_req(tf = getfree());
        copycell (formalargs, nextf);
	nextf->lefptr = NULL;
	/* put the new element at top of binding list */
	tf->lefptr = binlptr;
	binlptr = tf;
	tf->r.rigptr = nextf;
	/* only do pointing if not null */
	if (isnullcell(actualargs)==FALSE) {
		nextf->lefptr = temp = getfree();
		temp->r.rigptr = actualargs;	
	}
	mark_not(nextf);
	mark_not(tf);
	return 1; /* have only put one arg on binding list */

} /* end function bind_uneval */




/* Parallel binding, shared by lambda_bind and lx_let. */
/* All the values are evaluated before any variable is bound, so a value */
/* that names one of the new variables sees the caller's value */
/* e.g. (f 'x a) with (defun f (a b) ..) gives b the caller's a, not x, */
/* and (let ((a 1) (b a)) ..) gives b the outer a. */
/* bind_to_pending evaluates one value and collects the new binding on */
/* pending (kept gcflagged); bind_pending then puts them all on the top */
/* of the binding list together, in the same order as if pushed one at a */
/* time, so the last variable is at the top. */

static int bind_to_pending (SLC *var, SLC *valexpr, SLC **pending,
				SLC **pendlast, char *fname)
{
SLC *nextf, *nexta, *tf, *nexta_raw;

/* returns 1 if a binding was added to pending, 0 if var is not an atom */

mark_req(nextf = getfree());
mark_req(tf = getfree());
mark_req(nexta = getfree());
copycell (var, nextf);
nextf->lefptr = 0; /* cut link to next var */
if (nextf->lstat != IDATOM) {
	report_error (fname, "formal arguments must be atoms", NULL, FALSE);
	trace = TRUE;
	mark_not(tf);
	mark_not(nextf);
	mark_not(nexta);
	return 0;
}
copycell (valexpr, nexta); /* a NULL valexpr gives a null cell, so nil */
nexta->lefptr = 0;
nexta_raw = nexta; /* keep a handle on the pre-eval scratch cell */
mark_req(nexta = lx_eval(nexta)); /* eval the value */
mark_not(nexta_raw); /* release it now that eval is done reading it */
/* put the new element at top of the pending list */
tf->lefptr = *pending;
*pending = tf;
if (*pendlast == NULL) {
	*pendlast = tf;	/* first var, will link to binlptr */
}
tf->r.rigptr = nextf;
if (isnullcell(nexta)==FALSE) {
	nextf->lefptr = nexta;	/* only do pointing if not null */
} else {
	nextf->lefptr = NULL;
	mark_not(nexta);	/* not linked in, so not kept */
}
return 1;
} /* end function bind_to_pending */



static void bind_pending (SLC *pending, SLC *pendlast)
{
SLC *tf;

/* now bind them all, the binding list keeps them from here on */
if (pending) {
	pendlast->lefptr = binlptr;
	binlptr = pending;
	for (tf = pending; tf != pendlast->lefptr; tf = tf->lefptr) {
		mark_not(tf->r.rigptr->lefptr);	/* the value, may be NULL */
		mark_not(tf->r.rigptr);
		mark_not(tf);
	}
}
} /* end function bind_pending */



int lambda_bind (SLC *formalargs, SLC *actualargs)
{
SLC *pending, *pendlast;
int numbound;

/* does the binding of the lambda arguments onto the binding list */
/* returns number of arguments bound */
/* binds each element with an evaluated actual arg, in parallel, */
/* see bind_to_pending */

numbound = 0;
pending = pendlast = NULL;
/* formalargs is a list pointer */
formalargs = formalargs->r.rigptr;
while (isnullcell(formalargs)==FALSE) {
	if (!bind_to_pending(formalargs, actualargs, &pending, &pendlast, "lambda")) {
		break;
	}
	numbound++;
	/* move down to next formal and actual arg */
	formalargs = formalargs->lefptr;
	if (actualargs->lefptr != 0) {
		actualargs = actualargs->lefptr;
	} else if (formalargs) {
		/* another formal exists, but no actual given */
		report_error("lambda", "mismatch between actual and formal args", actualargs, TRUE);
		trace = TRUE;
		actualargs = getfree();
	}
} /* end loop */

bind_pending(pending, pendlast);
return numbound;
} /* end function lambda_bind */





SLC *do_lambda(SLC *inptr, SLC *form)
{
SLC *formalargs,*actionptr,*actualargs,*res, *temp, *actiontop;
int nbound;

mark_req(actualargs= getfree());
temp = inptr->r.rigptr;

/* actual args are found in inptr, following lambda def */
/* copycell copies across a 0 if src cell pointer is zero */
copycell (temp->lefptr,actualargs);

/* formal args follow the lambda keyword */ 
copycell ((form->r.rigptr)->lefptr,formalargs = getfree());
mark_req(formalargs);
if (formalargs) {
	actionptr = formalargs->lefptr;
	formalargs->lefptr = 0;
} else {
	actionptr = NULL;
}
actiontop = actionptr;
mark_req(actiontop);

/* bind depending on formal arg type */
if (isnullcell(formalargs)) {
	nbound = 0;	/* no formal args supplied */
} else if (formalargs->lstat == NUMATOM) {
	report_error ("lambda", "formal argument must not be a number", NULL, FALSE);
	trace = TRUE;
	nbound = 0;
} else if (formalargs->lstat == IDATOM) {
	nbound = bind_uneval(formalargs, actualargs);
} else {
	nbound = lambda_bind (formalargs,actualargs); /* a list of values */
} /* end decision of formalargs type */

res = NULL;
while (actionptr) {
	res = lx_eval(actionptr);
	actionptr = actionptr->lefptr;
}

/* does the unbinding of the lambda from the bind list top */
while (nbound--) {
	if (binlptr) {
		binlptr = binlptr->lefptr;
	} else {
		printf ("Error: unbind from empty binding list\n");
		longjmp (main_env,4);
	}
}
mark_not(actualargs);
mark_not(formalargs);
mark_not(actiontop);
return res;

} /* end function do_lambda */



/* functions to support property lists */

void add_pname(SLC *propname);
void add_idname(SLC *propptr, SLC *idnam);
SLC* sear_pname (SLC *pnam);
SLC* sear_idname(SLC *propptr, SLC *idnam);



void add_pname(SLC *propnam)
{
SLC *newcell;

/* given the  name adds an entry for the property name given.*/
/*  There are initally no values on the property */
/* property list is always garbage coll marked */
/* so no need to mark in this procedure */


newcell = getfree();
copycell (propnam, newcell);

/* splice into the property list */
newcell->lefptr = prlptr;
prlptr = newcell;

} /* end function add_pname */





void add_idname(SLC *propptr, SLC *idnam)
{
SLC *idcell, *lcell;

/* given the 'property' property list entry adds an entry */
/* for the identifier idnam given.  The initial property value is null */
/* property list is always garbage coll marked */
/* so no need to mark in this procedure */

/* splice into the property list */
lcell = getfree();
lcell->lefptr = propptr->lefptr;
propptr->lefptr = lcell;
/* add the cell holding the identifier */
idcell = getfree();
lcell->r.rigptr = idcell;
copycell (idnam, idcell);
idcell->lefptr = NULL;

} /* end function add_idname */






SLC *sear_pname(SLC *pnam)
{
SLC *current;

/* searches the property list for a property name */
/* returns a pointer to the name entry if found */
/* else returns NULL */

current = prlptr;
while (current) {
	if (current->lstat == pnam->lstat
            && current->r.rigptr == pnam->r.rigptr ){
		return current;
	}
	current = current->lefptr;
}
return NULL;
} /* end function sear_pname */



SLC *sear_idname(SLC *propptr, SLC *idnam)
{
SLC *current, *idcell;

/* given the 'property' property list entry searches list */
/* for an identifier entry matching idnam */
/* returns a pointer to the name entry if found else returns NULL */

current = propptr->lefptr;
while (current) {
	if (current->lstat != LSLST) return NULL; /* found next property */
	idcell = current->r.rigptr;
	if (idcell->lstat == idnam->lstat && idcell->r.rigptr == idnam->r.rigptr) {
		return current;
	}
	current = current->lefptr;
}
return NULL;

} /* end function sear_idname */





SLC *lx_put(SLC *form)
{
SLC *a1id,*a2prop,*a3val, *identptr, *propptr, *idcell;

/* used as put id propname value */
/* e.g. (put 'a 'mark1 34) */
/* id and propname must be atoms (possible numeric) */

a1id = form->lefptr;
a2prop = (a1id) ? a1id->lefptr :NULL;
a3val = (a2prop) ? a2prop->lefptr :NULL;
mark_req(a1id = lx_eval(a1id));
mark_req(a2prop = lx_eval(a2prop));
mark_req(a3val = lx_eval(a3val));
if (a1id == NULL || a2prop == NULL
    ||a1id->lstat == LSLST || a2prop->lstat == LSLST) {
	a3val = report_error ("put", "Id or Prop name not an atom", form, TRUE);
	goto exit;
}
/* search for the property name */
while ((propptr = sear_pname(a2prop))== NULL) {
	add_pname(a2prop);  /* if not found, add to list */

}
/* search for the id entry */
while ((identptr = sear_idname(propptr,a1id))== NULL) {
	add_idname(propptr,a1id);  /* if not found, add to list */

}
/* put on the new value */
idcell = identptr->r.rigptr;
idcell->lefptr = (a3val) ? a3val : NULL;

exit:
mark_not(a1id);
mark_not(a2prop);
mark_not(a3val);
return a3val;
} /* end function lx_put */





SLC *lx_get(SLC *form)
{
SLC *a1id,*a2prop,*a3val, *identptr, *propptr, *idcell;

/* used as get id propname */
/* e.g. (get 'a 'mark1) */
/* id and propname must be atoms */
/* returns the property found, or NULL if not found */

a1id = form->lefptr;
a2prop = (a1id) ? a1id->lefptr :NULL;
a3val = NULL;
mark_req(a1id = lx_eval(a1id));
mark_req(a2prop = lx_eval(a2prop));

if (a1id == NULL || a2prop == NULL
    ||a1id->lstat == LSLST || a2prop->lstat == LSLST) {
	a3val = report_error ("get", "Id or Prop name not an atom", form, TRUE);
} else 	if ((propptr = sear_pname(a2prop))!= NULL) {
	/* exists - now search for the id entry */
	if ((identptr = sear_idname(propptr,a1id))!= NULL) {
		/* identifier has got property */
		idcell = identptr->r.rigptr;
		a3val = idcell->lefptr;
	} /* end if id found */
} /* end if property found */

mark_not(a1id);
mark_not(a2prop);
return a3val;
} /* end function lx_get */





SLC *lx_remprop(SLC *form)
{
SLC *a1id,*a2prop, *identptr, *propptr, *next, *retval;

/* used as remprop id propname */
/* e.g. (remprop 'a 'mark1) */
/* id and propname must be atoms */
/* removes the atoms property, returning NULL */

retval = NULL;
a1id = form->lefptr;
a2prop = (a1id) ? a1id->lefptr :NULL;
mark_req(a1id = lx_eval(a1id));
mark_req(a2prop = lx_eval(a2prop));

if (a1id == NULL || a2prop == NULL
    ||a1id->lstat == LSLST || a2prop->lstat == LSLST) {
	retval = report_error ("remprop", "Id or Prop name not an atom", form, TRUE);
	goto exit;
} else 	if ((propptr = sear_pname(a2prop))!= NULL) {
	/* exists - now search for the id entry */
	if ((identptr = sear_idname(propptr,a1id))!= NULL) {
		/* identifier has got property */
		/* scan down property list to find where identptr is */
		next = prlptr;
		while (next) {
			if (next->lefptr == identptr) {
				/* chop out of list */
				next->lefptr = identptr->lefptr;
				break;
			}
			next = next->lefptr;
		} /* end loop */
	} /* end if id found */
} /* end if property found */

exit:
mark_not(a1id);
mark_not(a2prop);
return retval;
} /* end function lx_remprop */




SLC *lx_append(SLC *form)
{
SLC *a1ptr,*a2ptr, *oldtemp, *newtemp, *res, *wkptr;
int n;

/* the result is always a new list, a copy of the top level of both */
/* arguments, so changing it (rplaca, rplacd) changes neither of them. */
/* A missing argument, or one that evaluates to (), is an empty list: */
/* (append () ()) gives () */

a1ptr = form->lefptr;
a2ptr = (a1ptr) ? a1ptr->lefptr :NULL;

mark_req(a1ptr = lx_eval(a1ptr));
mark_req(a2ptr = lx_eval(a2ptr));

if ((a1ptr != NULL && a1ptr->lstat != LSLST)
    || (a2ptr != NULL && a2ptr->lstat != LSLST)) {
	res = report_error ("append", "args must eval to lists", form, TRUE);
	mark_not(a1ptr);
	mark_not(a2ptr);
	return NULL;
}

mark_req(oldtemp = res = getfree());
/* copy down the list a1ptr, then the list a2ptr, onto the result */
for (n = 0; n < 2; n++) {
	wkptr = (n == 0) ? a1ptr : a2ptr;
	wkptr = (wkptr) ? wkptr->r.rigptr : NULL;
	while (wkptr) {
		copycell (wkptr, newtemp = getfree());
		newtemp->lefptr = NULL;
		oldtemp->lefptr = newtemp;
		oldtemp = newtemp;
		wkptr = wkptr->lefptr;
	}
}

/* make res a proper list */
res->r.rigptr = res->lefptr;
res->lefptr = NULL;
mark_not(a1ptr);
mark_not(a2ptr);
mark_not(res);
if (res->r.rigptr == NULL) {
	return NULL; /* both lists were empty */
}
return res;

} /* end function lx_append */





SLC *lx_reverse(SLC *inptr)
{
/* returns a list consisting of the reversed input list  */
/* returns null if not a list given, arg given is already evaluated */

SLC *res, *oldtemp, *newtemp;

oldtemp = NULL;
if (inptr && inptr->lstat == LSLST && inptr->r.rigptr) {
	/* move to first ele of list */
	inptr = inptr->r.rigptr; 
	while (inptr) {
		mark_req(oldtemp);
		copycell(inptr,newtemp = getfree());
		newtemp->lefptr = oldtemp;
		mark_not(oldtemp);
		oldtemp = newtemp;			
		inptr = inptr->lefptr;
	} /* end loop */
} /* end if list */
mark_req(oldtemp);
res = getfree();
res->r.rigptr = oldtemp;
mark_not(oldtemp);
return res;
}/* end function lx_reverse */




SLC *lx_write(SLC *form, int spflag, int escflag)
{
/* used as:   write handle expression  */
/* file must already be open for writing */
/* returns the expression */

FILE *outfp;
SLC *a1ptr, *a2ptr;

a1ptr = form->lefptr;
a2ptr = (a1ptr) ? a1ptr->lefptr :NULL;
a1ptr = lx_eval(a1ptr);
if (a1ptr == NULL || a1ptr->lstat != NUMATOM) {
	return report_error ("write", "arg missing or not file handle", form, TRUE);
}
outfp = a1ptr->r.rigfp;
if (a1ptr->isfptr == 1 && outfp == NULL) {
	sprintf(outbuf, "Warning: write: file is closed\n");
	condpr(stdout);
	return NULL;
}
lx_prin(outfp,a2ptr = lx_eval(a2ptr), spflag, escflag);
return a2ptr;

} /* end function lx_write */





SLC *lx_rplaca(SLC *form)
{
SLC *a1ptr,*a2ptr, *newcar, *rest, *retval;

a1ptr = form->lefptr;
a2ptr = (a1ptr) ? a1ptr->lefptr :NULL;

if (a1ptr == NULL || a2ptr == NULL) {
	return report_error ("rplaca", "must have two arguments", form, TRUE);
}
mark_req(a1ptr = lx_eval(a1ptr));
if (a1ptr == NULL || a1ptr->lstat != LSLST) {
	retval = report_error ("rplaca", "first arg must eval to a list", a1ptr, TRUE);
	goto exit;
}
if (a1ptr->r.rigptr == NULL) {
	retval = report_error ("rplaca", "first arg must not be an empty list", a1ptr, TRUE);
	goto exit;
}
mark_req(a2ptr = lx_eval(a2ptr));
/* overwrite the first element's own cell with a copy of the value (a null */
/* cell for ()), keeping its link to the rest of the list. The cell, not the */
/* list header a1ptr, is what is shared with the list it belongs to: (cdr x) */
/* and (car x) hand back a fresh header, so repointing the header would not */
/* change x. The value's own cell may be a variable's value or part of a */
/* form, so it is copied, never linked into the list */
newcar = a1ptr->r.rigptr;
rest = newcar->lefptr;
copycell (a2ptr, newcar);
newcar->lefptr = rest;
retval = a1ptr;

exit:
mark_not(a1ptr);
mark_not(a2ptr);
return retval;

} /* end function lx_rplaca */






SLC *lx_rplacd(SLC *form)
{
SLC *a1ptr,*a2ptr, *listcar, *newcdr, *retval;

a1ptr = form->lefptr;
a2ptr = (a1ptr) ? a1ptr->lefptr :NULL;

if (a1ptr == NULL || a2ptr == NULL) {
	return report_error ("rplacd", "must have two arguments", form, TRUE);
}
mark_req(a1ptr = lx_eval(a1ptr));
if (a1ptr == NULL || a1ptr->lstat != LSLST) {
	retval = report_error ("rplacd", "first arg must eval to a list", a1ptr, TRUE);
	goto exit;
}
if (a1ptr->r.rigptr == NULL) {
	retval = report_error ("rplacd", "first arg must not be an empty list", a1ptr, TRUE);
	goto exit;
}
/* move down to first ele of list, which we know is not null */
listcar = a1ptr->r.rigptr;
mark_req(a2ptr = lx_eval(a2ptr));
/* and do the dirty work, the new rest of the list as lx_cons makes it */
if (a2ptr == NULL) {
	listcar->lefptr = NULL;
} else if (a2ptr->lstat == LSLST) {
	/* a list: its elements are the rest, (rplacd '(a b c) '(f g)) is (a f g) */
	listcar->lefptr = a2ptr->r.rigptr;
} else {
	/* an atom: a copy of it is the rest. Its own cell may be a */
	/* variable's value or part of a form, so must not be linked in */
	newcdr = getfree();
	copycell (a2ptr, newcdr);
	newcdr->lefptr = NULL;
	listcar->lefptr = newcdr;
}
mark_not(a2ptr);
retval = a1ptr;

exit:
mark_not(a1ptr);
return retval;

} /* end function lx_rplacd */






SLC *lx_ordinal(SLC *inptr)
{
/* wants form already evaluated */

SLC *res;

if (inptr == NULL || inptr->lstat != IDATOM) {
	return report_error ("ordinal", "requires an atom", inptr, TRUE);
}
res = getfree();
res->lstat = NUMATOM;
res->r.rigval =  (float) (*getident(inptr->r.idval));
return res;

} /* end function lx_ordinal */


SLC *lx_defined(SLC *arg)
{
  /* (defined x) gives true if the atom x is already defined (it has a value,
     or is a primitive), else null.  The argument is not evaluated when it is
     an atom, so x can be an atom that is not defined yet.  A list, e.g.
     (defined 'x), is evaluated and the atom it gives is tested. */
SLC *name = arg;

if (name != NULL && name->lstat != IDATOM) {
	name = lx_eval(name);
}
if (name == NULL || name->lstat != IDATOM) {
	return report_error ("defined", "requires an atom", name, TRUE);
}
return lx_eval_internal(name, true);

}  /* end lx_defined */







SLC* lx_system(SLC *inptr)
{
/* enable use of os facilities, allowing a string to be passed to os */
char *id;
SLC *res;
int systemresult;

if (inptr == NULL || inptr->lstat != IDATOM) {
	return report_error ("system", "arg must be string", inptr, TRUE);
} else {
		id = getident(inptr->r.idval);
		systemresult = system( id);
		res = getfree();
		res->lstat = NUMATOM;
		res->r.rigval = (float)systemresult;
		return res;
	}
} /* end function lx_system */



SLC* lx_implode(SLC *inptr)
{
/* produces an atom of the list supplied, returns null if list null */
/* list must contain identifiers, of which the first character is used only */
/* if the list contains a number, the character corresponding to the */
/* ASCII coding of hte number is used.  Not unichar. */
/* i.e. CR is (implode 10 13 ), the is (implode (list 't 'h 101)) */


SLC *res, *nextptr;
char newid[MAXIDLEN], *id;
int slen;

if (isnullcell(inptr)) return NULL;
if (inptr->lstat != LSLST) {
	return report_error ("implode", "argument must be a list", inptr, TRUE);
}
if (!inptr->r.rigptr) return NULL;
nextptr = inptr->r.rigptr;
slen = 0;
id = newid;
while(nextptr && slen < MAXIDLEN) {
	switch (nextptr->lstat) {
		case IDATOM:
			*id++ = *getident(nextptr->r.idval);
			slen++;
			break;
		case NUMATOM:
			*id++ = (char)((int)nextptr->r.rigval & 0x007f);
			slen++;
			break;
		default:
			break;/* no action */
	} /* end switch */
nextptr = nextptr->lefptr;
} /* end loop */
if (slen == 0) return NULL; /* no string length */
*id = 0; /* end string */
res = getfree();
res->lstat = IDATOM;
res->r.idval = putident(newid);
return res;
} /* end function lx_implode */


FILE *testfp(SLC *inptr);

FILE *testfp(SLC *inptr)
{
/* if the argument is present it is taken as a file handle */
/* returns NULL, with a warning, for a handle that has been closed */

if (!isnullcell(inptr) && inptr->lstat == NUMATOM && inptr->isfptr == 1) {
	if (inptr->r.rigfp == NULL) {
		sprintf(outbuf, "Warning: file is closed\n");
		condpr(stdout);
	}
	return (inptr->r.rigfp);
} else {
	return (inStream);
}
} /* end function testfp */


SLC* lx_read(SLC *inptr)
{
/* reads one s-expression from stdin or a file */
/* if the argument is present it is taken as a file handle */
/* if EOF found, then the expression read is null */
SLC *retval = NULL;
FILE *fp = testfp(inptr);

if (fp == NULL) {
	return NULL; /* closed file */
}
lex_sexp(fp, &retval);
return retval;
} /* end function lx_read */





SLC* lx_readch(SLC *inptr)
{
/* reads a single character from file, making an atom of it */
/* if the argument is present it is taken as a file handle */

SLC *res;
char c[5];
int tempc;
FILE *fp = testfp(inptr);

if (fp == NULL) {
	return NULL; /* closed file */
}
tempc = fgetc(fp);
if (tempc == EOF) {
	return NULL;
}
c[0] = (char)tempc;
c[1] = 0;
res = getfree();
res->lstat = IDATOM;
res->r.idval = putident(c);
return res;

} /* end function lx_readch */



SLC *lx_eof (SLC *inptr)
{
/* returns true if file is at end */
/* if the argument is present it is taken as a file handle */
/* a closed file counts as at the end */
FILE *fp = testfp(inptr);

if (fp == NULL || feof(fp)) {
	return lx_true();
} else {
	return NULL;
} /* end if */
} /* end function lx_eof */





SLC *lx_open (SLC *form)
{
/* opens for writing the specified file */

FILE *infp;
char *fname, *openmode;
SLC *a1ptr,*a2ptr, *res;

a1ptr = form->lefptr;
a2ptr = (a1ptr) ? a1ptr->lefptr :NULL;

a1ptr = lx_eval(a1ptr);
if (isnullcell(a1ptr) || a1ptr->lstat != IDATOM) {
	return report_error ("open", "first arg must be a string", form, TRUE);
}
/* select file open mode here, if any a2 argument at all, write else read */
openmode = (a2ptr) ? "w" : "r";
fname = getident(a1ptr->r.idval);	
	if((infp = fopen(fname, openmode)) == NULL){
		sprintf( outbuf, "Error: cannot find or open file %s\n", fname);
		condpr (stdout);
		return NULL;
	} 
	res = getfree();
	res->lstat = NUMATOM;
	res->isfptr = 1;
	res->r.rigfp = infp;
	return res;

} /* end function lx_open */




SLC *lx_close(SLC *inptr)
{
/* closes the specified file handle - returns NULL */

FILE *infp;

	if (isnullcell(inptr)
		 || inptr->lstat != NUMATOM
                 || inptr->isfptr == 0) {
		return report_error ("close", "must be given a file handle", inptr, TRUE);
	}
	infp = inptr->r.rigfp;
	if (infp == NULL) {
		/* closed already; closing again would free the FILE twice */
		sprintf( outbuf, "Warning: close: file is already closed\n");
		condpr (stdout);
		return NULL;
	}
	if (fclose(infp) != 0) {
		sprintf( outbuf, "Error: failed to close file\n");
		condpr (stdout);
	}
	/* mark the handle closed: still a file handle, but with no FILE, */
	/* so later close/read/write calls can tell and do not use freed memory */
	inptr->r.rigfp = NULL;
	return NULL;
} /* end function lx_close */

void formatnumberforprint(char* inbuf, SLC *inptr)
{
float fval;
int   ival;

	fval = inptr->r.rigval;
	ival = 10000.0 * fval -
		(10000.0 * roundf(fval));
	if (abs(ival) < 10) {
		sprintf(inbuf,"%ld",lround(fval));
	} else {
		sprintf(inbuf,"%.4f",fval);
	}

} /* end function formatnumberforprint */





SLC* lx_explode(SLC *inptr)
{
/* produces a list of an atom supplied */
/* if an number atom, produces list of chars for the number*/

SLC *res, *nextptr;
char nbuf[MAXIDLEN+1], *id, c[5] ; /* c used to store a 1 char string */

if (isnullcell(inptr) || inptr->lstat == LSLST) {
	return report_error ("explode", "must be given atom or number", inptr, TRUE);
}
if (inptr->lstat == IDATOM) {
	id = getident(inptr->r.idval);
	strcpy(nbuf, id);
	id = nbuf;
} else {
	/* its a number */
        formatnumberforprint(nbuf, inptr);
	id = nbuf;
}
mark_req(res = getfree()); /* the start of the result list */
nextptr = getfree(); /* first ele of result list */
res->r.rigptr  = nextptr; /* link the two together */
while (*id) {
	c[0] = *id;
	c[1] = '\0';  /* make the single char c string */
	nextptr->lstat = IDATOM;
	nextptr->r.idval = putident(c); /* store it away */
	id++; /* move string pointer to next letter */
	if (*id) {
		nextptr->lefptr = getfree();
		nextptr = nextptr->lefptr;
	}
} /* end loop */
mark_not(res);
return res;

} /* end function lx_explode */








SLC *lx_plus (SLC *form, int fn)
{
float total = 0.0;
float divisor = 0.0;
int j = 0;
SLC *wkptr,*arg,*evalarg;

/* does arithmetic on input, input pointing to form */
/* fn gives the operation to do */

if (fn == SQRT) {
	if (isnullcell(form->lefptr) == TRUE) {
		return report_error ("sqrt", "requires one argument", form, TRUE);
	}
	arg = form->lefptr;
	evalarg = lx_eval(arg);
	if (evalarg == NULL || evalarg->lstat != NUMATOM) {
		return report_error ("sqrt", "argument not numeric", evalarg, TRUE);
	}
	if (arg->lefptr != NULL) {
		return report_error ("sqrt", "too many arguments", form, TRUE);
	}
	if (evalarg->r.rigval < 0) {
		return report_error ("sqrt", "argument must not be negative", evalarg, TRUE);
	}
	wkptr = getfree();
	wkptr->lstat = NUMATOM;
	wkptr->r.rigval = sqrtf(evalarg->r.rigval);
	return wkptr;
}

if (isnullcell(form->lefptr)== FALSE) {
	arg = form->lefptr;
	do {
		evalarg = lx_eval(arg);
		if (evalarg == NULL || evalarg->lstat != NUMATOM) {
			return report_error ("arithmatic", "argument not numeric", evalarg, TRUE);
		} 
		if (j == 0) {
			total = evalarg->r.rigval;
			j++;
		} else {
			switch (fn) {
				case PLUS :
					total += evalarg->r.rigval;
					break;
				case DIFFERENCE :
					total -= evalarg->r.rigval;
					break;
				case TIMES :
					total *= evalarg->r.rigval;
					break;
				case DIVIDE :
					divisor = evalarg->r.rigval;
					if (divisor) {
						total /= divisor;
					} else {
						sprintf( outbuf, "Error: attempt to divide by 0\n");
						condpr (stdout);
						trace = TRUE;
					}
			} /* end fn switch */
		}
		arg = arg->lefptr;
	} while (arg);
} /* end if */
wkptr = getfree();
wkptr->lstat = NUMATOM;
wkptr->r.rigval = total;
return wkptr;
} /* end function lx_plus */







SLC *lx_cond(SLC *form)
{
SLC *currentterm,*testptr,*actionptr;

currentterm = form->lefptr;
while (currentterm) {
	if (currentterm->lstat != LSLST) {
		return report_error ("cond", "not a list following", form, TRUE);
	} /* end error check */
	testptr = currentterm->r.rigptr;
	actionptr = (testptr) ? testptr->lefptr : NULL ;
	testptr = lx_eval(testptr);
	if (isnullcell(testptr) == FALSE) {
		while (actionptr) {
			/* loop to eval all arguments in cond list */
			testptr = lx_eval(actionptr);
			actionptr = actionptr->lefptr;
		}
		return testptr; /* returning the last evaluated one */
		/* or if only test exists, returns eval of test */
	}
	currentterm = currentterm->lefptr;
} /* end loop for cond terms */
return NULL;
} /* end function cond */





SLC *lx_eq(SLC *form)
{
SLC *a1,*a2;

/* returns true if the two (evaluated) args are the */
/* same numeric, or same atom, or same listcell */

a1 = form->lefptr;
a2 = (a1) ? a1->lefptr : NULL;
a1 = lx_eval(a1);
mark_req (a1);
a2 = lx_eval(a2);
mark_not (a1);
if (isnullcell(a1) && isnullcell(a2)) {
	return lx_true();
}
if (isnullcell(a1) || isnullcell(a2)){
	return NULL;
}
if (a1->lstat == a2->lstat
   && a1->r.rigptr == a2->r.rigptr) {
	return lx_true();
}
return NULL; /* not equal */
} /* end function lx_eq */



SLC *lx_obl(SLC *inptr)
{
SLC *temp;

/* various housekeeping type things */
/* no arg : returns entire oblist, as a total list */
/* arg = 1: returns entire property list */
/*       2: string storage info printed  */
/*       3: forces grabage collection    */
/*       4: forces break and return to command level */
/*       5: turns on garbage collection announcing   */
/*       6: turns off garbage collection announcing  (default)  */
/*       7: turns on evaluation tracing, for current evaluation only  */
/*       8: turns off evaluation tracing, (default) */
/*       9: prints out the binding list (only useful in a bound environment) */
/*      10: returns list of primitive names */
/*      11: forces program exit (for use in scripting) */

temp = getfree(); /* to return a standard list of the result */
if (inptr != NULL && inptr->lstat == NUMATOM) {
	switch ((int)inptr->r.rigval) {
		case 1:	temp->r.rigptr = prlptr;
			break;
		case 2: 
			/* stats on string storage */
			sprintf( outbuf, "Chars %d,  Ids %d out of %d\n",atomcharsused,atomidcount,atomcap);
			condpr (stdout);
			break;
		case 3:
			garbage_coll(FALSE);
			temp = NULL;
			break;
		case 4:
			longjmp(main_env, 2); /* will exit from this procedure */
		case 5:
			garb_announce = TRUE;
			break;
		case 6:
			garb_announce = FALSE;
			break;
		case 7:
			trace = TRUE;
			break;
		case 8:
			trace = FALSE;
			break;
                case 9:
			temp->r.rigptr = binlptr; 
			break;
		case 10:
			temp =  lx_helpfunc();
			break;
		default:
			exit(0);
			break;
		} /* end switch */
} else {
	temp->r.rigptr = oblptr; /* return whole oblist */
}
return temp;
} /* end function lx_obl */


SLC *lx_helpfunc ()
{
int i;
SLC *tail, *temp, *rethead;
	mark_req(rethead = getfree());
	tail = getfree();
	rethead->lstat = LSLST;
	rethead->r.rigptr = tail; 
	for (i = 1; i <= maxprims ; i++)
	{
		tail->lstat = IDATOM;
		tail->isfptr = 0;
		tail->r.idval = i;
		if (i != maxprims) {
			temp = getfree();
			tail->lefptr = temp;
			tail = temp;
		}
	}
	mark_not(rethead);
	return rethead;
}



SLC *lx_cdr(SLC *form)
{
SLC *newptr,*le1, *inptr;

if (isnullcell(form->lefptr)) {
	return report_error ("cdr", "must have an argument", form, TRUE);
}
mark_req (newptr = getfree());
inptr = lx_eval(form->lefptr);
mark_not(newptr);
/* return null if null evaluated arg */
if (inptr == NULL) {
	return NULL;
}
if (inptr->lstat != LSLST) {
	return report_error ("cdr", "argument must be a list", inptr, TRUE);
}
/* make le1 point at first list element, if any */
le1 = inptr->r.rigptr;
if (le1) {
	/* only process if le1 cell exists */
	if (le1->lefptr) {
		/* there is a 'rest of list' */
		/* so make newptr point at it, newptr being the list node */
		newptr->r.rigptr = le1->lefptr;
	}
}
return newptr;
} /* end function lx_cdr */




SLC *lx_car(SLC *form)
{
SLC *res, *inptr;

if (isnullcell(form->lefptr)) {
	return report_error ("car", "must have an argument", form, TRUE);
}
inptr = lx_eval(form->lefptr);
if (inptr == NULL) {
	/* return null for a null car evaluated argument */
	return NULL;
}
if (inptr->lstat != LSLST) {
	return report_error ("car", "car argument must be a list", inptr, TRUE);
}
mark_req(inptr);
copycell (inptr->r.rigptr,res = getfree());
mark_not(inptr);
res->lefptr = NULL; /* cut link to rest of list */
return res;
} /* end function lx_car */





SLC *lx_cons(SLC *form)
{
SLC *a1ptr,*a2ptr,*newptr, *res, *tail;


a1ptr = form->lefptr;
a2ptr = (a1ptr) ? a1ptr->lefptr :NULL;

if (a1ptr == NULL || a2ptr == NULL) {
	return report_error ("cons", "must have two args", form, TRUE);
}
mark_req(res = getfree());
newptr = getfree();
res->r.rigptr = newptr;
mark_req(a1ptr = lx_eval(a1ptr));
a2ptr = lx_eval(a2ptr);
if (a1ptr) {
	newptr->lstat = a1ptr->lstat;
	newptr->r = a1ptr->r;
}
if (a2ptr) {
	if (a2ptr->lstat == LSLST) {
		newptr->lefptr = a2ptr->r.rigptr;
	} else {
		/* atom: a copy of it is the last element. Its own cell may */
		/* be a variable's value or part of a form, so must not be */
		/* linked into the list */
		bool wasflagged = a2ptr->gcflagged;

		mark_req(a2ptr); /* getfree can run a garbage collection */
		tail = getfree();
		if (!wasflagged) {
			mark_not(a2ptr);
		}
		copycell (a2ptr, tail);
		tail->lefptr = NULL;
		newptr->lefptr = tail;
	} /* else a2 type test */
} 
mark_not(res);
mark_not(a1ptr);
return res;
} /* end function lx_cons */




SLC *lx_length(SLC *inptr)
{
/* returns a listcell containing the number of top level list elements */

long count;
SLC *wkptr;

count = 0L;
	/* must be a list for it to work */
	/* returns zero for an atom or null element */
if (inptr) {
	if (inptr->lstat == LSLST && inptr->r.rigptr) {
		/* move to first ele of list */
		inptr = inptr->r.rigptr; 
		do {
			count++;
			inptr = inptr->lefptr;
		} while (inptr);
	} /* end if list */
}
wkptr = getfree();
wkptr->lstat = NUMATOM;
wkptr->r.rigval = count;
return wkptr;
}/* end function lx_length */




SLC *lx_minusp(SLC *inptr)
{

/* returns true if the inptr is a numeric atom with a negative value */

if (inptr  && inptr->lstat == NUMATOM) {
	if (inptr->r.rigval < 0) {
		return lx_true();
	} else {
		return NULL;
	}
} else {
	return report_error ("minusp", "arg must be a number", inptr, TRUE);
}
} /* end function lx_minusp */




SLC *lx_listp(SLC *inptr)
{

/* returns true if the inptr is a list (may be null) */

if (inptr == NULL || inptr->lstat == LSLST ) {
	return lx_true();
} else {
	return NULL;
}
} /* end function lx_listp */





SLC *lx_atom(SLC *inptr)
{

/* returns true if the inptr is an atom, null is valid */

if (inptr == NULL || inptr->lstat != LSLST ) {
	return lx_true();
} else {
	return NULL;
}
} /* end function lx_atom */





SLC *lx_numberp(SLC *inptr)
{

/* returns true if the inptr is a numeric atom, null not valid */

if (inptr && inptr->lstat == NUMATOM ) {
	return lx_true();
} else {
	return NULL;
}
} /* end function lx_numberp */



SLC *lx_null(SLC *inptr)
{

/* returns true if the inptr is a null cell */

if (isnullcell(inptr)) {
	return lx_true();
} else {
	return NULL;
} /* end if */
} /* end function lx_null */




SLC *lx_true(void)
{
/* returns an atom with the predefined id of true, ie not null */

SLC *wkptr;

wkptr = getfree();
wkptr->lstat = IDATOM;
wkptr->r.idval = TRUEID;
return wkptr;
} /* end function lx_true */



SLC *lx_and (SLC *form)
{
/* returns last evaluated form if all forms evaluate to non-() */
/* returns the first result that is () */
SLC *res=NULL;

while (form->lefptr) {
	if (isnullcell((res = lx_eval(form->lefptr)))) {
		return res;
	}
	form = form->lefptr;
} /* end loop */
return res;
} /* end function lx_and */



SLC *lx_or (SLC *form)
{
/* returns first non-() evaluation of forms */
SLC *res=NULL;

while (form->lefptr) {
	if (!isnullcell((res = lx_eval(form->lefptr)))) {
	  return res;
        }
	form = form->lefptr;
} /* end loop */
return res;
} /* end function lx_or */



/* sets a block of local lexical variables up */
/* let ( (var1 val1) (var2 val2).. ) form1 form2   ) */
/* locates the var list, binds each var to its evaluated val */
/* locates the forms, evaluates each one */
/* unbinds the binding list at the end */
/* shared by let and let*, both using bind_to_pending/bind_pending as */
/* lambda_bind does. parallel (let): all the vals are evaluated before */
/* any var is bound, so in ((a 1) (b a)) b gets the outer a, as with */
/* lambda args. series (let*): each var is bound as soon as its val is */
/* evaluated, so in ((a 1) (b a)) b gets 1 */
static SLC *let_common(SLC *form, int parallel, char *fname)
{
  SLC *vlist, *flist, *clause, *var, *res;
  SLC *pending = NULL, *pendlast = NULL;
  int nbound = 0;
  int formcount = 0; /* if there is a let without any forms its useless */

  vlist = form->lefptr; /* process each (var val1) clause */
  /* check vlist before reading from it, a bare (let) has none */
  if (vlist == NULL || vlist->lstat != LSLST || vlist->r.rigptr == NULL ) {
	printf ("Error: %s: bad locals - needs ( (var val..) )\n", fname);
	longjmp (main_env,5);
  }
  clause = vlist->r.rigptr;
  while(clause) {
    if (clause->lstat != LSLST || clause->r.rigptr == NULL) {
      report_error (fname, "each local must be (var val)", clause, TRUE);
      trace = TRUE;
    } else {
      var = clause->r.rigptr;
      /* var->lefptr is the val, NULL for a clause of just (var) */
      if (bind_to_pending(var, var->lefptr, &pending, &pendlast, fname)) {
        nbound += 1;	/* only count a binding that was made */
        if (!parallel) {
          /* let*: bind now, so the following vals see this var */
          bind_pending(pending, pendlast);
          pending = pendlast = NULL;
        }
      }
    }
    clause = clause->lefptr;
  }
  bind_pending(pending, pendlast);
  /* evaluate form1 form2... to the end of the let */
  flist = vlist->lefptr;
  if (flist == NULL ) {
	printf ("Error: %s: wants ((var1 val2))...form1... Brackets wrong?\n", fname);
	longjmp (main_env,5);
  }
  res = NULL;
  while (flist) {
	res = lx_eval(flist);
	flist = flist->lefptr;
        formcount += 1;
  }
  /* take the elements off the binding list */
  while (nbound--) {
	if (binlptr) {
		binlptr = binlptr->lefptr;
	} else {
		printf ("Error:  %s: unbind from empty binding list\n", fname);
		longjmp (main_env,5);
	}
  }
  if (formcount == 0) {
	printf ("Error:  %s: no forms to evaluate (perhaps bracket error)\n", fname);
	longjmp (main_env,5);
  }
return res;
} /* end function let_common */


/* let: parallel binding */
SLC *lx_let(SLC *form)
{
  return let_common(form, TRUE, "let");
} /* end function lx_let */


/* let*: series binding */
SLC *lx_letstar(SLC *form)
{
  return let_common(form, FALSE, "let*");
} /* end function lx_letstar */



SLC *lx_set(SLC *form, int mode)
{
SLC *a1ptr,*a2ptr,*newptr,*tptr,*retval;

/* set (mode EVAL) and setq (mode NOEVAL) */
/* lx_prin(stdout,form, SPACE, NOESC); this is for debug */
copycell(form->lefptr, a1ptr = getfree());
mark_req(a1ptr); /* in case gc triggered by a2ptr free cell begin got */
copycell(a1ptr->lefptr,a2ptr = getfree());
a1ptr->lefptr = a2ptr->lefptr = NULL;
mark_not(a1ptr);
mark_req(a2ptr);

if (mode == EVAL ) {
	a1ptr = lx_eval(a1ptr); /* set evals both args */
} 
                  /* for the setq function arg 1 not evaluated */
mark_req(a1ptr);
mark_not(a2ptr);
a2ptr = lx_eval(a2ptr);
mark_req (a2ptr);
/* does the actual setting of the oblist, arguments are already evaluated */
/* only assigns if a1ptr has some value, removing old definition, if any */
if (a1ptr == NULL || a1ptr->lstat != IDATOM) {
	retval = report_error("set(q)","args must be non-numeric atoms",form, TRUE);
	goto exit;
} /* end error check */
/* search returns a null if nothing found; a set within a binding */
/* finds and changes the binding */
newptr= sear_oblist(a1ptr);
if (newptr == NULL) {
	/* add the new name element to the oblist */
	if (mode == EVAL) {
		/* set: the name cell is a copy of the first arg's value, */
		/* whose own cell may be a variable's value or part of a */
		/* form (setq's is a copy already) */
		tptr = getfree();
		copycell (a1ptr, tptr);
		tptr->lefptr = NULL;
		mark_not(a1ptr);
		mark_req(a1ptr = tptr);
	}
	newptr = getfree();
	newptr->lefptr = oblptr;
	oblptr = newptr;
	newptr->r.rigptr = a1ptr;
	oblcache_invalidate(); /* its cached "not in the oblist" is now wrong */
} 
tptr = newptr->r.rigptr; 
if (isnullcell(a2ptr)==FALSE) {
	/* only do pointing if not null definition */
	tptr->lefptr = a2ptr;
} else {
	tptr->lefptr = NULL;
}
retval = a2ptr;

exit:
mark_not (a1ptr);
mark_not (a2ptr);
return retval;
} /* end function lx_set */




SLC *lx_list(SLC *form)
{
SLC *aptr,*result, *rescar ,*temp;

/* returns a list of the evaluated arguments - passed the unevaluated form */

if (isnullcell(form->lefptr)) return NULL; /* no arguments in form */

mark_req(rescar = temp = getfree());

while (form->lefptr) {
	aptr = lx_eval(form->lefptr);
	if (aptr) {
		copycell (aptr, temp);
	}
	form = form->lefptr; /* move down to next ele of form list */
	if (form->lefptr) {
		temp->lefptr = getfree();
		temp = temp->lefptr; /* move down to next of result list */
	} /* end if more form */
} /* end loop */

temp->lefptr = NULL;
result = getfree(); /* header ele of result list */
result->r.rigptr = rescar;
mark_not(rescar);
return result;
} /* end function lx_list */




/* MAXLOOP is in listspec.h. compile_loop and compile_while in compex.c */
/* do as lx_loop and lx_while here: a change here needs the same there */
int looplevel = 0;
char loopgo[MAXLOOP];

SLC *lx_loop(SLC *form)
{
SLC *res, *curr;

/* evaluates all the terms of the form in turn, continually */
/* unless a while or until term is reached to stop the loop */

if (++looplevel >= MAXLOOP-1 ) {
	fprintf (stdout, "Error: loops nested too deep");
	longjmp(main_env,2);
}
res = NULL;
loopgo[looplevel] = TRUE;
while (loopgo[looplevel]) {
	check_keyboard();
	curr = form->lefptr;
	while (curr && loopgo[looplevel]) {
		res = lx_eval(curr);
		curr = curr->lefptr;
	}
} /* end outer loop */
looplevel--;
return res;
} /* end function lx_loop */



SLC *lx_while(SLC *form, int test)
{
SLC *res;

/* used for both while and until */
/* evaluates the form supplied */
/* if result same as test, sets the loopgo to stop the current loop level */
/* and evaluates the rest of the expressions in the while or until list */

res = lx_eval(form->lefptr);
if (isnullcell(res) == test) {
	loopgo[looplevel] = FALSE;
	form = form->lefptr;
	while (form->lefptr) {
		res = lx_eval(form->lefptr);
		form = form->lefptr;
	} /* end rest of expressions */
}
return res;
} /* end function lx_while */

