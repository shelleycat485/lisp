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

extern SLC *lx_eval                 (SLC *);
extern SLC *lx_car                  (SLC *);
extern SLC *lx_cdr                  (SLC *);
extern SLC *lx_cons                 (SLC *);
extern SLC *lx_reverse              (SLC *);
extern SLC *lx_append               (SLC *);
extern SLC *lx_length               (SLC *);
extern SLC *lx_minusp               (SLC *);
extern SLC *lx_true                 (void);
extern SLC *lx_listp                (SLC *);
extern SLC *lx_atom                 (SLC *);
extern SLC *lx_numberp              (SLC *);
extern SLC *lx_list                 (SLC *);
extern SLC *lx_loop                 (SLC *);
extern SLC *lx_while                (SLC *, int );
extern SLC *lx_null                 (SLC *);
extern SLC *lx_set                  (SLC *, int );
extern SLC *lx_let                  (SLC *);
extern SLC *lx_obl                  (SLC *);
extern SLC *lx_helpfunc             ();
extern SLC *lx_plus                 (SLC *, int );
extern SLC *lx_cond                 (SLC *);
extern SLC *lx_and                  (SLC *);
extern SLC *lx_or                   (SLC *);
extern SLC *lx_load                 (SLC *);
extern SLC *lx_eq                   (SLC *);
extern SLC *lx_eof                  (SLC *);
extern SLC *lx_readch               (SLC *);
extern SLC *lx_explode              (SLC *);
extern SLC *lx_implode              (SLC *);
extern SLC *lx_write                (SLC *, int , int );
extern SLC *lx_read                 (SLC *);
extern SLC *lx_open                 (SLC *);
extern SLC *lx_close                (SLC *);
extern SLC *lx_put                  (SLC *);
extern SLC *lx_remprop              (SLC *);
extern SLC *lx_get                  (SLC *);
extern SLC *lx_system               (SLC *);
extern SLC *lx_rplaca               (SLC *);
extern SLC *lx_rplacd               (SLC *);
extern SLC *lx_ordinal              (SLC *);
void read_file (char * fname);
void Prompt_and_Read(int fout);  
SLC *internal_set(SLC *form, int mode, int bindingflag);

int garb_announce = FALSE;
jmp_buf main_env;

void read_file (char *fname)
{
FILE *infilestream;
SLC *hptr; /* head of list to be evaluated */

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
	} /* end read file */
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


/* runs in a forked process */
void Prompt_and_Read(int fout){  
	FILE *outStream = fdopen(fout, "w");
	char *line;

	/* send */
	while(1) {
		line=linenoise("Lisp:");
		if ( line == NULL) {
			break;
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
char *libname;
/*int fd[2];*/
int fd1[2];
int pipe1 = pipe(fd1);
SLC *hptr; /* head of list to be evaluated */

printf("LISP. Copyright R Haxby 1991-2025. Version 3.10\n");
printf("This program comes with ABSOLUTELY NO WARRANTY;\n");
printf("This is free software, and you are welcome to redistribute it\n");
printf("under certain conditions");

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
set_control_c ();      /* set up control C handler here */

for(i=1; i < argc ; ++i){
	printf ("...reading file %s\n", argv[i]);
	read_file (argv[i]);
} /* end i loop */

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
	sprintf(outbuf, "\n Abort");
	condpr(stdout);
	fflush(inStream);
	ccc = fgetc(inStream);
	sprintf(outbuf, "\n after Abort got a %c\n", ccc);
	condpr(stdout);
	switch (ccc)
		{
			case 'A' :
			case 'a' :
			case EOF :
				longjmp(main_env,2);
			case 'T' :
			case 't' :
				trace = TRUE;
				lex_sexp(inStream, &retval);
				return retval;
			case 'R' :
			case 'r' :
				trace = FALSE;
				lex_sexp(inStream, &retval);
				return retval;
			default:
			       ;  /* do nothing */
		} /* end switch */
	} /* end loop */
} /* end function report_error */



int bind_uneval (SLC *formalargs,
		SLC *actualargs    );
int lambda_bind (SLC *formalargs,
		SLC *actualargs    );
SLC *do_lambda (SLC *inptr, SLC *form);

int trace; /* for switching on evaluation tracing */
int formname; /* for debug use, when getfree is called */

SLC *lx_eval (SLC *inptr)
{
SLC *form, *newform, *res, *res1;
int redefs;
	check_keyboard();  /* allow user break in here */
	if (isnullcell(inptr) ) return NULL; /* test for null input here */
	formname = EVALID;
	res = form = NULL;
	mark_req (inptr);

	if (inptr->lstat == NUMATOM) {
		res = inptr;
		goto endeval;
	} 

	if (inptr->lstat == IDATOM) {
		res1 = sear_oblist (inptr);
		if (res1 != NULL) {
			/* get the oblist entry value */
			res1 = res1->r.rigptr;
			res = res1->lefptr;
		} else if (inptr->r.idval > maxprims) {
			sprintf(outbuf, "Error: %s had no value here\n\r",getident(inptr->r.idval));
			condpr (stdout);
			trace = TRUE;
		} else {
			/* is a primitive */
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
		res = do_lambda (inptr, form);
		goto endeval;
	}


	/* check for "(3)" type of list */
	if (form->lstat != IDATOM) {
		report_error("eval","first element of a list must be a function", form, TRUE);
		goto endeval;
	}
	formname = form->r.idval;



	formname = form->r.idval;
	/* test for primitive names here */
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
			res = (lx_listp (lx_eval(form->lefptr)));
			break;
		case 26:
			res = (lx_numberp (lx_eval(form->lefptr)));
			break;
		case 27:
			res = (lx_atom (lx_eval(form->lefptr)));
			break;
		case 28:
		case 29:	
			res = (lx_null (lx_eval(form->lefptr))); /* not  and null */
			break;
		case 30:
			res = lx_length (lx_eval(form->lefptr));
			break;
		case 31: 
			res = lx_obl(lx_eval(form->lefptr));
			break;
		case 32:
			lx_prin(stdout,res = lx_eval(form->lefptr), SPACE, NOESC);
			/* print operation - final cr */
			break;
		case 33:
			lx_prin(stdout,res = lx_eval(form->lefptr), NOSPACE, NOESC);
			break;
		case 34:
			/* princ - print with special chars escaped in */
			lx_prin(stdout,res = lx_eval(form->lefptr), NOSPACE, ESC);
			break;
		case 35:
			lx_load(res = lx_eval(form->lefptr));
			break;
		case 36:
			res = lx_readch(lx_eval(form->lefptr));
			break;
		case 37:
			res = (lx_explode (lx_eval(form->lefptr)));
			break;
		case 38:
			res = lx_append(form);
			break;
		case 39:
			res = lx_read(lx_eval(form->lefptr));
			break;
		case 40:
			res = lx_open(form);
			break;
		case 41:
			res = lx_close(lx_eval(form->lefptr));
			break;
		case 42:
			res = lx_put(form);
			break;
		case 43:
			res = lx_remprop(form);
			break;
		case 44:
			res = lx_get(form);
			break;
		case 45:
			res = (lx_implode (lx_eval(form->lefptr)));
			break;
		case 46:
			res = lx_rplaca(form);
			break;
		case 47:
			res = lx_rplacd(form);
			break;
		case 48:
			/* writec - write with special chars escaped in */
			res = lx_write(form, SPACE, ESC);
			break;
		case 49:
			/* writen - output like prin */
			res = lx_write(form, NOSPACE, NOESC);
			break;
		case 50:
			res = lx_write(form, SPACE, NOESC); /* write */
			break;
		case 51:
			res = lx_reverse (lx_eval(form->lefptr));
			break;
		case 52:
			res = lx_eq(form);
			break;
		case 53:
			res = lx_initturtle (form);
			break;
		case 54:
			res = lx_home (form);
			break;
		case 55:
			res = lx_pendown (form);
			break;
		case 56:
			res = lx_setfill (form);
			break;
		case 57:
			res = lx_pencolour (form);
			break;
		case 58:
			res = lx_fillcolour (form);
			break;
		case 59:
			res = lx_turn (form);
			break;
		case 60:
			res = lx_turnto (form);
			break;
		case 61:
			res = lx_move (form);
			break;
		case 62:
			res = lx_moveto (form);
			break;
		case 63:
			res = lx_circle (form);
			break;
		case 64:
			res = lx_ellipse (form);
			break;
		case 65:
			res = lx_rectangle (form);
			break;
		case 66:
			res = lx_onscreen (form);
			break;
		case 67:
			res = lx_polygon (form);
			break;
		case 68:
			res = lx_let (form);
			break;
		default:
			report_error ("eval", "non translatable list name", form, TRUE);
			trace = TRUE;
			break;
	} /* end switch on primitive names */
endeval:

	if (trace) {
		sprintf (outbuf, "\nEval given (in trace now): ");
		condpr (stdout);
		lx_prin (stdout,inptr, SPACE, NOESC);
                if (trace++ == 10) {
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




int lambda_bind (SLC *formalargs, SLC *actualargs)
{
SLC *nextf, *nexta, *tf;
int numbound;

/* does the binding of the lambda arguments onto the binding list */
/* returns number of arguments bound */
/* binds each element with an evaluated actual arg */


numbound = 0;
/* formalargs is a list pointer */
formalargs = formalargs->r.rigptr;
while (isnullcell(formalargs)==FALSE) {
	mark_req(nextf = getfree());
	mark_req(tf = getfree());
	mark_req(nexta = getfree());
	copycell (formalargs, nextf);
	nextf->lefptr = 0; /* cut link to next arg */
	if (nextf->lstat != IDATOM) {
		report_error ("lambda", "formal arguments must be atoms", NULL, FALSE);
		trace = TRUE;
		break;
	}
	copycell (actualargs, nexta);
	nexta->lefptr = 0;
	mark_req(nexta = lx_eval(nexta)); /* eval the actual argument */
	/* put the new element at top of binding list */
	tf->lefptr = binlptr;
	binlptr = tf;
	/* add the new def to the binding list */
	tf->r.rigptr = nextf;
	if (isnullcell(nexta)==FALSE) {
		nextf->lefptr = nexta;	/* only do pointing if not null */
	} else {
		nextf->lefptr = NULL;
	}
	numbound++;
	mark_not(tf);
	mark_not(nextf);
	mark_not(nexta);
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
return numbound;
} /* end function lambda_bind */





SLC *do_lambda(SLC *inptr, SLC *form)
{
SLC *formalargs,*actionptr,*actualargs,*res, *temp;
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
mark_req(actionptr);

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
mark_not(actionptr);
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
SLC *a1id,*a2prop, *identptr, *propptr, *next;

/* used as remprop id propname */
/* e.g. (remprop 'a 'mark1) */
/* id and propname must be atoms */
/* removes the atoms property, returning NULL */

a1id = form->lefptr;
a2prop = (a1id) ? a1id->lefptr :NULL;
mark_req(a1id = lx_eval(a1id));
mark_req(a2prop = lx_eval(a2prop));

if (a1id == NULL || a2prop == NULL
    ||a1id->lstat == LSLST || a2prop->lstat == LSLST) {
	return report_error ("remprop", "Id or Prop name not an atom", form, TRUE);
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

mark_not(a1id);
mark_not(a2prop);
return NULL;
} /* end function lx_remprop */




SLC *lx_append(SLC *form)
{
SLC *a1ptr,*a2ptr, *oldtemp, *newtemp, *res, *wkptr;

a1ptr = form->lefptr;
a2ptr = (a1ptr) ? a1ptr->lefptr :NULL;

if (a1ptr == NULL || a2ptr == NULL) {
	return report_error ("append", "must have two arguments", form, TRUE);
}
mark_req(a1ptr = lx_eval(a1ptr));
a2ptr = lx_eval(a2ptr);

if (a2ptr == NULL) { /* if arg2 evals to (), nothing to append */
	mark_not(a1ptr);
	return a1ptr;
}

if (a1ptr == NULL) { /* if arg1 evals to (), nothing to prepend */
	mark_not(a1ptr);
	return a2ptr;
}

if (a1ptr->lstat != LSLST || a2ptr->lstat != LSLST) {
	res = report_error ("append", "args must eval to lists", form, TRUE);
	mark_not(a1ptr);
	return NULL;
}

mark_req(a2ptr);
mark_req(oldtemp = res = getfree());
/* copy down the list a1ptr, which is known to be a list now */
wkptr = a1ptr->r.rigptr;
while (wkptr) {
	copycell (wkptr, newtemp = getfree());
	oldtemp->lefptr = newtemp;
	oldtemp = newtemp;
	wkptr = wkptr->lefptr;
}
/* add on second list now */
oldtemp->lefptr = a2ptr->r.rigptr;

/* make res a proper list */
res->r.rigptr = res->lefptr;
res->lefptr = NULL;
mark_not(a1ptr);
mark_not(a2ptr);
mark_not(res);
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
lx_prin(outfp,a2ptr = lx_eval(a2ptr), spflag, escflag);
return a2ptr;

} /* end function lx_write */





SLC *lx_rplaca(SLC *form)
{
SLC *a1ptr,*a2ptr, *listcar, *listcdr;

a1ptr = form->lefptr;
a2ptr = (a1ptr) ? a1ptr->lefptr :NULL;

if (a1ptr == NULL || a2ptr == NULL) {
	return report_error ("rplaca", "must have two arguments", form, TRUE);
}
mark_req(a1ptr = lx_eval(a1ptr));
if (a1ptr == NULL || a1ptr->lstat != LSLST) {
	return report_error ("rplaca", "first arg must eval to a list", a1ptr, TRUE);
}
mark_req(a2ptr = lx_eval(a2ptr));
/* remember the rest of list a1ptr */
listcar = a1ptr->r.rigptr;
listcdr = listcar->lefptr;
/* and do the dirty work */
a1ptr->r.rigptr = a2ptr;
a2ptr->lefptr = listcdr;
mark_not(a1ptr);
mark_not(a2ptr);
return a1ptr;

} /* end function lx_rplaca */






SLC *lx_rplacd(SLC *form)
{
SLC *a1ptr,*a2ptr, *listcar;

a1ptr = form->lefptr;
a2ptr = (a1ptr) ? a1ptr->lefptr :NULL;

if (a1ptr == NULL || a2ptr == NULL) {
	return report_error ("rplacd", "must have two arguments", form, TRUE);
}
mark_req(a1ptr = lx_eval(a1ptr));
if (a1ptr == NULL || a1ptr->lstat != LSLST) {
	return report_error ("rplacd", "first arg must eval to a list", a1ptr, TRUE);
}
/* move down to first ele of list, which we know is not null */
listcar = a1ptr->r.rigptr;
/* and do the dirty work */
listcar->lefptr = lx_eval(a2ptr);
mark_not(a1ptr);
return a1ptr;

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

if (!isnullcell(inptr) && inptr->lstat == NUMATOM && inptr->isfptr == 1) {
	return (inptr->r.rigfp);
} else {
	return (stdin);
}
} /* end function testfp */


SLC* lx_read(SLC *inptr)
{
/* reads one s-expression from stdin or a file */
/* if the argument is present it is taken as a file handle */
/* if EOF found, then the expression read is null */
SLC *retval = NULL;

lex_sexp(testfp(inptr), &retval);
return retval;
} /* end function lx_read */





SLC* lx_readch(SLC *inptr)
{
/* reads a single character from file, making an atom of it */
/* if the argument is present it is taken as a file handle */

SLC *res;
char c[5];
int tempc;

tempc = fgetc(testfp(inptr));
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

if (feof(testfp(inptr))) {
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
	if (fclose(infp) != 0) {
		sprintf( outbuf, "Error: failed to close file\n");
		condpr (stdout);
	}
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
			sprintf( outbuf, "Chars %d out of %d,  Ids %d out of %d\n",idstuse,MAXID,idcount,MAXNUMIDS);
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
	return report_error ("car", "argument must be a list", inptr, TRUE);
}
mark_req(inptr);
copycell (inptr->r.rigptr,res = getfree());
mark_not(inptr);
res->lefptr = NULL; /* cut link to rest of list */
return res;
} /* end function lx_car */





SLC *lx_cons(SLC *form)
{
SLC *a1ptr,*a2ptr,*newptr, *res;


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
		/* atom */
		newptr->lefptr = a2ptr;
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
/* set ( (var1 val1) (var2 val2).. ) form1 form2   ) */
/* locates the var list, assignes each to binding list like set */
/* locates the forms, evaluates each one */
/* unbinds the binding list at the end */
SLC *lx_let(SLC *form)
{
  SLC *vlist, *flist, *a1ptr, *a2ptr, *res, *s1, *s2;
  int nbound = 0;
  int formcount = 0; /* if there is a let without any forms its useless */

  s1 = getfree(); /* set up the start of a setq form */
  mark_req(s1);
  s1->lstat=LSLST;
  s2= getfree();
  s2->r.idval=16;  /* the one for setq but not checked so does not matter */
  s2->lstat=IDATOM;
  s1->r.rigptr = s2;

  vlist = form->lefptr; /* process each (var val1) clause */
  a1ptr = vlist->r.rigptr;
  if (vlist == NULL || vlist->lstat != LSLST || a1ptr == NULL ) {
	printf ("Error: let: bad locals - needs ( (var val..) )\n");
	longjmp (main_env,5);
  }
  while(a1ptr) {
    copycell(a1ptr, a2ptr = getfree());
    s2->lefptr = a2ptr->r.rigptr;
    a2ptr->lefptr = NULL;
    /* lx_prin(stdout,s1, SPACE, NOESC);  for debug */
    internal_set(s1->r.rigptr, NOEVAL, 1);  
    a1ptr = a1ptr->lefptr;
    nbound += 1;
  }
  mark_not(s1);
  /* evaluate form1 form2... to the end of the let */
  flist = vlist->lefptr;
  if (flist == NULL ) {
	printf ("Error: let: wants ((var1 val2))...form1... Brackets wrong?\n");
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
		printf ("Error:  let: unbind from empty binding list\n");
		longjmp (main_env,5);
	}
  }
  if (formcount == 0) {
	printf ("Error:  let: no forms to evaluate (perhaps bracket error)\n");
	longjmp (main_env,5);
  }
return res;
} /* end function lx_let */



SLC *lx_set(SLC *form, int mode)
{
  return internal_set(form, mode, 0);

} /* end function internal_set */



SLC *internal_set(SLC *form, int mode, int bindingflag)
{
SLC *a1ptr,*a2ptr,*newptr,*tptr;

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
	return report_error("set(q)","args must be non-numeric atoms",form, TRUE);
} /* end error check */
/* search returns a null if nothing found */
newptr = NULL;
if (!bindingflag) {
  newptr= sear_oblist(a1ptr); /* only search if a set, if a binding always */
                              /* want a new entry to be made.  A set function */
                              /* in a binding will still want a search */
}
if (newptr == NULL) {
	/* add the new name element to the oblist */
        /* also called internally from let, which uses the binding list */
	newptr = getfree();
	if (bindingflag) {
	    newptr->lefptr = binlptr;
	    binlptr = newptr;
	} else { 
	    newptr->lefptr = oblptr;
	    oblptr = newptr;
 	}
	newptr->r.rigptr = a1ptr;
} 
tptr = newptr->r.rigptr; 
if (isnullcell(a2ptr)==FALSE) {
	/* only do pointing if not null definition */
	tptr->lefptr = a2ptr;
} else {
	tptr->lefptr = NULL;
}
mark_not (a1ptr);
mark_not (a2ptr);
return a2ptr;
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




#define MAXLOOP 50
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


