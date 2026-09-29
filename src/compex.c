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
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include "listspec.h"
#include "turtinf.h"

typedef uint8_t byte;
#define CODEMEM_SIZE 1000 /* size of the executable area used by compex */
#define ARENA_SIZE 65536 /* permanent executable area for function bodies */




/* ---- compex: compile a form to machine code in an executable area ---- */
/* The generated code makes the same calls, in the same order, as the */
/* switch in lx_eval_internal (main.c). Constants (form pointers, function */
/* addresses) are placed inline, so the code is position independent. */

typedef void (*anyfn)(void);   /* generic function pointer, cast when used */
enum {
	SP_NONE,      /* f(form [, params]) or, if evalarg, f(value of arg) */
	SP_QUOTE,     /* the value is the constant form->lefptr */
	SP_EVAL,      /* lx_eval(value of arg): eval evaluates twice */
	SP_PRINT,     /* lx_prin(stdout, value of arg, params); result the value */
	SP_RETARG,    /* f(value of arg); result the value (system, load) */
	SP_UNEVALARG, /* f(form->lefptr), arg not evaluated (defined) */
	SP_COND,      /* cond: clauses compiled, with jumps */
	SP_ARITH,     /* + - * /: arguments compiled, totalled by cx_arith_* */
	SP_SQRT,      /* sqrt: first argument compiled, then cx_sqrt */
	SP_AND,       /* and: arguments compiled, jump out at the first () */
	SP_OR,        /* or: arguments compiled, jump out at the first non-() */
	SP_EQ,        /* eq: both arguments compiled, then cx_eq_* */
	SP_FALLBACK   /* not compiled: lx_eval(whole form) */
};
typedef struct {
	const char *name;  /* for the trace printout */
	anyfn fn;          /* NULL = not compiled, fall back to lx_eval(form) */
	bool evalarg;      /* true: compile the first argument, pass its value */
	int nparams;       /* 0, 1 or 2 extra int parameters */
	int paramval;
	int paramval2;
	int special;
} primentry;

static byte *emit_p;   /* next byte to write */
static byte *emit_end; /* end of the executable area */
static bool emit_ok;   /* false once the area has overflowed */

static void emit_bytes(const void *src, size_t n)
{
if (emit_p + n > emit_end) {
	emit_ok = false;
	return;
}
memcpy(emit_p, src, n);
emit_p += n;
} /* end function emit_bytes */

static void emit32(uint32_t w) { emit_bytes(&w, 4); }
static void emit64(uint64_t q) { emit_bytes(&q, 8); }

/* the generated function keeps one saved value in a stack slot, used to */
/* keep a value across a call (print, system, load); a save is always */
/* followed by its restore before any other code is compiled */
#if defined(__aarch64__)
/* x0 is both the first argument and the result register */
static void emit_prologue(void)
{
emit32(0xa9be7bfd); /* stp x29, x30, [sp, #-32]! (slot at sp+16) */
emit32(0x910003fd); /* mov x29, sp */
}
static void emit_epilogue(void)
{
emit32(0xa8c27bfd); /* ldp x29, x30, [sp], #32 */
emit32(0xd65f03c0); /* ret */
}
static void emit_const_x0(uint64_t k)
{
emit32(0x58000040); /* ldr x0, #8 */
emit32(0x14000003); /* b #12 (over the constant) */
emit64(k);
}
static void emit_const_arg(uint64_t k)    { emit_const_x0(k); }
static void emit_const_result(uint64_t k) { emit_const_x0(k); }
/* extra int parameter n (1, 2 or 3) into w1, w2 or w3 */
static void emit_param(int n, int p)
{
emit32(0x52800000 | (((uint32_t)p & 0xffff) << 5) | (uint32_t)n); /* movz wn, #p */
}
static void emit_call(uint64_t fn)
{
emit32(0x58000050); /* ldr x16, #8 */
emit32(0x14000003); /* b #12 */
emit64(fn);
emit32(0xd63f0200); /* blr x16 */
}
static void emit_const_arg2(uint64_t k)
{
emit32(0x58000041); /* ldr x1, #8 */
emit32(0x14000003); /* b #12 */
emit64(k);
}
static void emit_result_to_arg(void)  { }                  /* x0 already */
static void emit_result_to_arg2(void) { emit32(0xaa0003e1); } /* mov x1, x0 */
static void emit_save_result(void)    { emit32(0xf9000be0); } /* str x0, [sp, #16] */
static void emit_restore_result(void) { emit32(0xf9400be0); } /* ldr x0, [sp, #16] */
/* forward jumps: emitted with a zero offset, the returned site is */
/* patched once the target is known */
static byte *emit_jump_if_int_false(void)
{
byte *site = emit_p;
emit32(0x34000000); /* cbz w0, <patched> */
return site;
}
static byte *emit_jump_if_int_true(void)
{
byte *site = emit_p;
emit32(0x35000000); /* cbnz w0, <patched> */
return site;
}
static byte *emit_jump(void)
{
byte *site = emit_p;
emit32(0x14000000); /* b <patched> */
return site;
}
static void patch_jump(byte *site, byte *target)
{
uint32_t w;
int32_t off = (int32_t)((target - site) / 4);
memcpy(&w, site, 4);
if ((w & 0xfc000000) == 0x14000000) {
	w |= (uint32_t)off & 0x03ffffff;          /* b: imm26 */
} else {
	w |= ((uint32_t)off & 0x7ffff) << 5;       /* cbnz: imm19 */
}
memcpy(site, &w, 4);
}
#define CODE_SUPPORTED 1
#elif defined(__x86_64__)
/* rdi, rsi, edx, ecx are the arguments, rax the result */
static void emit8(uint8_t b) { emit_bytes(&b, 1); }
static void emit_prologue(void)
{
emit8(0x55);                                    /* push rbp */
emit8(0x48); emit8(0x83); emit8(0xec); emit8(0x10); /* sub rsp, 16 (slot at rsp) */
}
static void emit_epilogue(void)
{
emit8(0x48); emit8(0x83); emit8(0xc4); emit8(0x10); /* add rsp, 16 */
emit8(0x5d); emit8(0xc3);                       /* pop rbp; ret */
}
static void emit_const_arg(uint64_t k)
{
emit8(0x48); emit8(0xbf); emit64(k); /* movabs rdi, k */
}
static void emit_const_result(uint64_t k)
{
emit8(0x48); emit8(0xb8); emit64(k); /* movabs rax, k */
}
/* extra int parameter n (1, 2 or 3) into esi, edx or ecx */
static void emit_param(int n, int p)
{
static const uint8_t op[4] = { 0, 0xbe, 0xba, 0xb9 }; /* mov r32, imm32 */
emit8(op[n]); emit32((uint32_t)p);
}
static void emit_call(uint64_t fn)
{
emit8(0x48); emit8(0xb8); emit64(fn); /* movabs rax, fn */
emit8(0xff); emit8(0xd0);             /* call rax */
}
static void emit_const_arg2(uint64_t k)
{
emit8(0x48); emit8(0xbe); emit64(k); /* movabs rsi, k */
}
static void emit_result_to_arg(void)
{
emit8(0x48); emit8(0x89); emit8(0xc7); /* mov rdi, rax */
}
static void emit_result_to_arg2(void)
{
emit8(0x48); emit8(0x89); emit8(0xc6); /* mov rsi, rax */
}
static void emit_save_result(void)
{
emit8(0x48); emit8(0x89); emit8(0x04); emit8(0x24); /* mov [rsp], rax */
}
static void emit_restore_result(void)
{
emit8(0x48); emit8(0x8b); emit8(0x04); emit8(0x24); /* mov rax, [rsp] */
}
/* forward jumps: emitted with a zero offset, the returned site (the */
/* rel32 field) is patched once the target is known */
static byte *emit_jump_if_int_false(void)
{
byte *site;
emit8(0x85); emit8(0xc0);  /* test eax, eax */
emit8(0x0f); emit8(0x84);  /* jz rel32 */
site = emit_p;
emit32(0);
return site;
}
static byte *emit_jump_if_int_true(void)
{
byte *site;
emit8(0x85); emit8(0xc0);  /* test eax, eax */
emit8(0x0f); emit8(0x85);  /* jnz rel32 */
site = emit_p;
emit32(0);
return site;
}
static byte *emit_jump(void)
{
byte *site;
emit8(0xe9);               /* jmp rel32 */
site = emit_p;
emit32(0);
return site;
}
static void patch_jump(byte *site, byte *target)
{
int32_t off = (int32_t)(target - (site + 4)); /* from the next instruction */
memcpy(site, &off, 4);
}
#define CODE_SUPPORTED 1
#else
#define CODE_SUPPORTED 0
#endif

/* compex prints its results, trace and compiled function locations */
/* only if the environment variable LISPCSPRINT is set (to anything); */
/* set in lx_compex on each call */
static bool cx_print = false;

#if CODE_SUPPORTED
static void trace_step(int depth, const char *what, SLC *arg)
{
int i;
if (!cx_print) {
	return;
}
for (i = 0; i < depth; i++) {
	sprintf(outbuf, "  "); condpr(stdout);
}
sprintf(outbuf, "%s ", what); condpr(stdout);
if (arg) {
	SLC one = *arg;   /* print just this cell, not the rest of its list */
	one.lefptr = NULL;
	lx_prin(stdout, &one, SPACE, NOESC);
}
sprintf(outbuf, "\n"); condpr(stdout);
} /* end function trace_step */


/* ---- compiled user functions ---- */
/* A user function (defun f (args) body...) is compiled once, the first */
/* time compiled code calls it, and kept: its body goes into a permanent */
/* executable area and the entry is found by f's atom id. A call to f in */
/* compiled code calls cx_call, which binds the arguments exactly as */
/* do_lambda does (lambda_bind, or bind_uneval for (defun f lis ...)), */
/* runs the compiled body and unbinds. Assumes a compiled function is */
/* never redefined or edited. */

typedef struct cfunc {
	int atomid;            /* the function name's atom id */
	SLC *lambda;           /* its (lambda (args) body...) definition */
	byte *code;            /* where its compiled body is, NULL until done */
	size_t len;            /* length of the compiled body in bytes */
	struct cfunc *nextpending; /* waiting to be compiled */
} cfunc;

static cfunc **ctable = NULL;  /* compiled functions, indexed by atom id */
static cfunc *pendingfns = NULL; /* registered, body not yet compiled */
static byte *arena = NULL;     /* executable area for function bodies */
static size_t arena_used = 0;

/* the (lambda ...) list a head atom is defined as, or NULL */
static SLC *lambda_of(SLC *head)
{
SLC *def = sear_oblist(head);

if (def == NULL) {
	return NULL;
}
def = def->r.rigptr->lefptr; /* the value, as in lx_eval_internal */
if (def != NULL && def->lstat == LSLST && def->r.rigptr != NULL
    && def->r.rigptr->lstat == IDATOM && def->r.rigptr->r.idval == LAMID) {
	return def;
}
return NULL;
} /* end function lambda_of */

/* the table entry for function id, registering it (body not yet */
/* compiled) if it is new; NULL if the table cannot be made */
static cfunc *cfunc_for(int id, SLC *lam)
{
cfunc *cf;

if (ctable == NULL) {
	ctable = calloc(MAXATOMS, sizeof *ctable);
	if (ctable == NULL) {
		puts("Fatal: compex: No compiled function table allocate");
		exit(8);
	}
}
if (id < 0 || id >= MAXATOMS) {
	return NULL;
}
if (ctable[id] != NULL) {
	return ctable[id]; /* compiled (or registered) already */
}
cf = calloc(1, sizeof *cf);
if (cf == NULL) {
	puts("Fatal: compex: No compiled function entry allocate");
	exit(8);
}
cf->atomid = id;
cf->lambda = lam;
cf->nextpending = pendingfns;
pendingfns = cf;
ctable[id] = cf;
return cf;
} /* end function cfunc_for */

/* called from compiled code for (f args...): inptr is the call, cf is f */
/* binds as do_lambda (main.c) does, runs f's compiled body, unbinds */
static SLC *cx_call(SLC *inptr, cfunc *cf)
{
SLC *formalargs, *actualargs, *res;
SLC *(*body)(void);
int nbound;

mark_req(actualargs = getfree());
/* actual args follow the function name in the call */
copycell(inptr->r.rigptr->lefptr, actualargs);
/* formal args follow the lambda keyword */
copycell((cf->lambda->r.rigptr)->lefptr, formalargs = getfree());
mark_req(formalargs);
formalargs->lefptr = 0;

if (isnullcell(formalargs)) {
	nbound = 0; /* no formal args */
} else if (formalargs->lstat == NUMATOM) {
	report_error ("lambda", "formal argument must not be a number", NULL, FALSE);
	trace = TRUE;
	nbound = 0;
} else if (formalargs->lstat == IDATOM) {
	nbound = bind_uneval(formalargs, actualargs);
} else {
	nbound = lambda_bind(formalargs, actualargs); /* a list of values */
}

memcpy(&body, &cf->code, sizeof body); /* data pointer -> function pointer */
res = body();

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
return res;
} /* end function cx_call */


/* ---- compiled calls with compiled arguments ---- */
/* For (f a b ...) where f's formals are a list of atoms and there are */
/* at least as many args as formals, the args are compiled. As in */
/* lambda_bind (main.c), all the values are worked out before any */
/* formal is bound: each value is made into a binding at once but kept */
/* on a pending list, gcflagged, then cx_frame_call puts them all on */
/* the binding list together, runs f's body and unbinds. Pending lists */
/* are on a stack, one per call whose args are being worked out, so a */
/* call in an argument, (f (g 1) (g 2)), has its own. Extra args are */
/* not evaluated, as in lambda_bind */

#define MAXFRAMES 10000
static SLC *frame_pending[MAXFRAMES]; /* newest binding first */
static SLC *frame_last[MAXFRAMES];    /* the first binding made */
static int  frame_count[MAXFRAMES];
static int  frame_depth = 0;

static void cx_frame_begin(void)
{
if (frame_depth >= MAXFRAMES - 1) {
	puts("Fatal: compex: compiled calls nested too deep");
	exit(9);
}
frame_depth++;
frame_pending[frame_depth] = NULL;
frame_last[frame_depth] = NULL;
frame_count[frame_depth] = 0;
} /* end function cx_frame_begin */

/* makes the binding of formal (an atom cell) to value, on the pending */
/* list of the current frame, as bind_to_pending (main.c) does */
static void cx_frame_arg(SLC *value, SLC *formal)
{
SLC *nextf, *tf;

mark_req(value); /* first: getfree can run a garbage collection */
mark_req(nextf = getfree());
mark_req(tf = getfree());
copycell(formal, nextf);
nextf->lefptr = 0;
tf->lefptr = frame_pending[frame_depth];
frame_pending[frame_depth] = tf;
if (frame_last[frame_depth] == NULL) {
	frame_last[frame_depth] = tf; /* first formal, will link to binlptr */
}
tf->r.rigptr = nextf;
if (isnullcell(value) == FALSE) {
	nextf->lefptr = value;
} else {
	nextf->lefptr = NULL;
	mark_not(value); /* not linked in, so not kept */
}
frame_count[frame_depth]++;
} /* end function cx_frame_arg */

/* binds the pending bindings, runs f's compiled body, unbinds */
static SLC *cx_frame_call(cfunc *cf)
{
SLC *pending = frame_pending[frame_depth];
SLC *pendlast = frame_last[frame_depth];
SLC *tf, *res;
SLC *(*body)(void);
int nbound = frame_count[frame_depth];

frame_depth--;
if (pending) {
	/* as bind_pending (main.c): all onto the binding list together */
	pendlast->lefptr = binlptr;
	binlptr = pending;
	for (tf = pending; tf != pendlast->lefptr; tf = tf->lefptr) {
		mark_not(tf->r.rigptr->lefptr); /* the value, may be NULL */
		mark_not(tf->r.rigptr);
		mark_not(tf);
	}
}
memcpy(&body, &cf->code, sizeof body); /* data pointer -> function pointer */
res = body();
while (nbound--) {
	if (binlptr) {
		binlptr = binlptr->lefptr;
	} else {
		printf ("Error: unbind from empty binding list\n");
		longjmp (main_env,4);
	}
}
return res;
} /* end function cx_frame_call */

/* the number of formals if they are a list of atoms and the call has */
/* at least that many args, else -1 (then cx_call and lambda_bind do it) */
static int compiled_arg_count(SLC *call, cfunc *cf)
{
SLC *formals = (cf->lambda->r.rigptr)->lefptr; /* the (a b ...) cell */
SLC *f, *a;
int n = 0;

if (formals == NULL || formals->lstat != LSLST || formals->r.rigptr == NULL) {
	return -1; /* (lambda lis ...) or no formals: args not evaluated */
}
a = call->r.rigptr->lefptr;
for (f = formals->r.rigptr; f != NULL; f = f->lefptr) {
	if (f->lstat != IDATOM || a == NULL) {
		return -1;
	}
	a = a->lefptr;
	n++;
}
return n;
} /* end function compiled_arg_count */


/* ---- compiled eq ---- */
/* The first argument's value is kept, gcflagged as lx_eq (main.c) */
/* does, on a stack (one per eq being worked out, so nested eqs have */
/* their own) while the second is worked out; then the two are */
/* compared as lx_eq compares them */

#define MAXEQ 10000
static SLC *eq_first[MAXEQ];
static int  eq_depth = 0;

static void cx_eq_first(SLC *value)
{
if (eq_depth >= MAXEQ - 1) {
	puts("Fatal: compex: compiled eq nested too deep");
	exit(9);
}
mark_req(value);
eq_first[++eq_depth] = value;
} /* end function cx_eq_first */

static SLC *cx_eq_second(SLC *a2)
{
SLC *a1 = eq_first[eq_depth--];

mark_not(a1);
if (isnullcell(a1) && isnullcell(a2)) {
	return lx_true();
}
if (isnullcell(a1) || isnullcell(a2)) {
	return NULL;
}
if (a1->lstat == a2->lstat && a1->r.rigptr == a2->r.rigptr) {
	return lx_true();
}
return NULL; /* not equal */
} /* end function cx_eq_second */

static void compile(SLC *x, primentry *tab, int depth);


/* ---- compiled + - * / and sqrt ---- */
/* Each argument's code is followed by a call to cx_arith_add with its */
/* value, which works it into a running total as lx_plus (main.c) does, */
/* in float. The totals are kept on a stack, one per + - * or / being */
/* worked out, so nested sums and recursion each have their own; they */
/* are plain numbers, so nothing needs protecting from garbage */
/* collection. A non-numeric argument reports lx_plus's error and the */
/* code jumps straight to cx_arith_end, skipping the rest, as lx_plus */
/* returns at once. */

#define MAXARITH 10000 /* nesting depth of + - * / being worked out */
static float arith_total[MAXARITH];
static int   arith_count[MAXARITH];
static SLC  *arith_error[MAXARITH]; /* report_error's value, if any */
static bool  arith_failed[MAXARITH];
static int   arith_depth = 0;

static void cx_arith_begin(void)
{
if (arith_depth >= MAXARITH - 1) {
	puts("Fatal: compex: compiled arithmetic nested too deep");
	exit(9);
}
arith_depth++;
arith_total[arith_depth] = 0.0;
arith_count[arith_depth] = 0;
arith_error[arith_depth] = NULL;
arith_failed[arith_depth] = false;
} /* end function cx_arith_begin */

/* returns 1 if the value is not a number (then the rest is skipped) */
static int cx_arith_add(SLC *value, int fn)
{
if (value == NULL || value->lstat != NUMATOM) {
	arith_error[arith_depth] = report_error("arithmatic", "argument not numeric", value, TRUE);
	arith_failed[arith_depth] = true;
	return 1;
}
if (arith_count[arith_depth]++ == 0) {
	arith_total[arith_depth] = value->r.rigval;
	return 0;
}
switch (fn) {
	case PLUS:
		arith_total[arith_depth] += value->r.rigval;
		break;
	case DIFFERENCE:
		arith_total[arith_depth] -= value->r.rigval;
		break;
	case TIMES:
		arith_total[arith_depth] *= value->r.rigval;
		break;
	case DIVIDE:
		if (value->r.rigval) {
			arith_total[arith_depth] /= value->r.rigval;
		} else {
			/* as lx_plus: report, leave the total, carry on */
			sprintf( outbuf, "Error: attempt to divide by 0\n");
			condpr (stdout);
			trace = TRUE;
		}
		break;
}
return 0;
} /* end function cx_arith_add */

/* sqrt of the compiled value of its first argument, with lx_plus's */
/* checks in lx_plus's order; form is the (sqrt ...) form's head cell */
static SLC *cx_sqrt(SLC *value, SLC *form)
{
SLC *res;

if (value == NULL || value->lstat != NUMATOM) {
	return report_error ("sqrt", "argument not numeric", value, TRUE);
}
if (form->lefptr->lefptr != NULL) {
	return report_error ("sqrt", "too many arguments", form, TRUE);
}
if (value->r.rigval < 0) {
	return report_error ("sqrt", "argument must not be negative", value, TRUE);
}
res = getfree();
res->lstat = NUMATOM;
res->r.rigval = sqrtf(value->r.rigval);
return res;
} /* end function cx_sqrt */

static SLC *cx_arith_end(void)
{
SLC *res;

if (arith_failed[arith_depth]) {
	res = arith_error[arith_depth];
} else {
	res = getfree();
	res->lstat = NUMATOM;
	res->r.rigval = arith_total[arith_depth];
}
arith_depth--;
return res;
} /* end function cx_arith_end */

/* compiles (+ a b ...), (- ...), (* ...) or (/ ...); false if it has */
/* too many args */
#define MAXARITHARGS 64
static bool compile_arith(SLC *head, primentry *pe, primentry *tab, int depth)
{
SLC *arg;
byte *errjumps[MAXARITHARGS];
int n = 0, i;
char what[80];

for (arg = head->lefptr; arg != NULL; arg = arg->lefptr) {
	if (++n > MAXARITHARGS) {
		return false;
	}
}
n = 0;
snprintf(what, sizeof what, "%s:", pe->paramval == PLUS ? "+" : pe->paramval == DIFFERENCE ? "-"
	: pe->paramval == TIMES ? "*" : "/");
trace_step(depth, what, NULL);
emit_call((uintptr_t)cx_arith_begin);
for (arg = head->lefptr; arg != NULL; arg = arg->lefptr) {
	compile(arg, tab, depth + 1);
	emit_result_to_arg();
	emit_param(1, pe->paramval);
	emit_call((uintptr_t)cx_arith_add);
	errjumps[n++] = emit_jump_if_int_true(); /* not a number: stop */
}
if (emit_ok) {
	for (i = 0; i < n; i++) {
		patch_jump(errjumps[i], emit_p);
	}
}
emit_call((uintptr_t)cx_arith_end);
return true;
} /* end function compile_arith */

/* compiles (cond (test form...) ...) as lx_cond (main.c) does it: the */
/* first clause whose test is not () gives the value of its last form, */
/* or of the test if it has no forms; () if no test is true. Each test */
/* is followed by a jump to the next clause if the test is (). Returns */
/* false, having written nothing, if a clause is not a list (then the */
/* caller calls lx_cond, which reports the error) or there are too many */
#define MAXCLAUSES 64
static bool compile_cond(SLC *head, primentry *tab, int depth)
{
SLC *clause, *test, *body;
byte *endjumps[MAXCLAUSES];
byte *nextjump;
int n = 0, i;

for (clause = head->lefptr; clause != NULL; clause = clause->lefptr) {
	if (clause->lstat != LSLST || ++n > MAXCLAUSES) {
		return false;
	}
}
n = 0;
trace_step(depth, "cond:", NULL);
for (clause = head->lefptr; clause != NULL; clause = clause->lefptr) {
	test = clause->r.rigptr;          /* NULL for a clause () */
	body = (test) ? test->lefptr : NULL;
	trace_step(depth + 1, "test", test);
	compile(test, tab, depth + 2);
	if (body == NULL) {
		emit_save_result();       /* the test's value is the result */
	}
	emit_result_to_arg();
	emit_call((uintptr_t)isnullcell);
	nextjump = emit_jump_if_int_true(); /* test was (): next clause */
	if (body == NULL) {
		emit_restore_result();
	}
	for (; body != NULL; body = body->lefptr) {
		compile(body, tab, depth + 2);
	}
	endjumps[n++] = emit_jump();       /* to the end of the cond */
	if (emit_ok) {
		patch_jump(nextjump, emit_p);
	}
}
trace_step(depth + 1, "no test true: const ()", NULL);
emit_const_result(0);
if (emit_ok) {
	for (i = 0; i < n; i++) {
		patch_jump(endjumps[i], emit_p);
	}
}
return true;
} /* end function compile_cond */

/* compiles (and a b ...) or (or a b ...) as lx_and / lx_or (main.c): */
/* each argument but the last is followed by a test of its value, and */
/* and stops at the first () value, or at the first non-() value; the */
/* value that stopped it is the result. The last argument needs no test: */
/* its value is the result either way. No arguments gives (). The saved */
/* value is restored at the shared exit it jumps to. Returns false if */
/* there are too many arguments */
#define MAXLOGICARGS 64
static bool compile_andor(SLC *head, bool isand, primentry *tab, int depth)
{
SLC *arg;
byte *outjumps[MAXLOGICARGS];
byte *endjump;
int n = 0, i;

for (arg = head->lefptr; arg != NULL; arg = arg->lefptr) {
	if (++n > MAXLOGICARGS) {
		return false;
	}
}
trace_step(depth, isand ? "and:" : "or:", NULL);
if (head->lefptr == NULL) {
	trace_step(depth + 1, "no args: const ()", NULL);
	emit_const_result(0);
	return true;
}
n = 0;
for (arg = head->lefptr; arg->lefptr != NULL; arg = arg->lefptr) {
	compile(arg, tab, depth + 1);
	emit_save_result();
	emit_result_to_arg();
	emit_call((uintptr_t)isnullcell);
	/* and: out if the value was (); or: out if it was not () */
	outjumps[n++] = isand ? emit_jump_if_int_true() : emit_jump_if_int_false();
}
compile(arg, tab, depth + 1); /* the last: its value is the result */
endjump = emit_jump();
if (emit_ok) {
	for (i = 0; i < n; i++) {
		patch_jump(outjumps[i], emit_p);
	}
}
emit_restore_result(); /* the value that stopped it */
if (emit_ok) {
	patch_jump(endjump, emit_p);
}
return true;
} /* end function compile_andor */

/* writes code that leaves the value of cell x in the result register */
static void compile(SLC *x, primentry *tab, int depth)
{
SLC *head;
primentry *pe;
char what[80];

if (x == NULL) {
	trace_step(depth, "const ()", NULL);
	emit_const_result(0);
	return;
}
if (x->lstat == LSLST && x->r.rigptr != NULL) {
	head = x->r.rigptr;
	if (head->lstat == IDATOM && head->r.idval >= 1 && head->r.idval <= maxprims
	    && sear_oblist(head) == NULL) {
		pe = &tab[head->r.idval];
		switch (pe->special) {
		case SP_QUOTE:
			trace_step(depth, "const", head->lefptr);
			emit_const_result((uintptr_t)head->lefptr);
			return;
		case SP_EVAL:
			compile(head->lefptr, tab, depth + 1); /* do the argument first */
			trace_step(depth, "call lx_eval on the value", NULL);
			emit_result_to_arg();
			emit_call((uintptr_t)lx_eval);
			return;
		case SP_PRINT:
			compile(head->lefptr, tab, depth + 1);
			snprintf(what, sizeof what, "call lx_prin(stdout, value, %d, %d) (%s), keep the value",
				pe->paramval, pe->paramval2, pe->name);
			trace_step(depth, what, NULL);
			emit_save_result();
			emit_result_to_arg2();
			emit_const_arg((uintptr_t)stdout);
			emit_param(2, pe->paramval);
			emit_param(3, pe->paramval2);
			emit_call((uintptr_t)lx_prin);
			emit_restore_result();
			return;
		case SP_RETARG:
			compile(head->lefptr, tab, depth + 1);
			snprintf(what, sizeof what, "call lx_%s on the value, keep the value", pe->name);
			trace_step(depth, what, NULL);
			emit_save_result();
			emit_result_to_arg();
			emit_call((uintptr_t)pe->fn);
			emit_restore_result();
			return;
		case SP_ARITH:
			if (compile_arith(head, pe, tab, depth)) {
				return;
			}
			/* too many arguments: call lx_plus with the form */
			snprintf(what, sizeof what, "call lx_plus, param %d:", pe->paramval);
			trace_step(depth, what, x);
			emit_const_arg((uintptr_t)head);
			emit_param(1, pe->paramval);
			emit_call((uintptr_t)lx_plus);
			return;
		case SP_EQ:
			/* (eq a b): a missing arg is (), extra args are not */
			/* evaluated, as in lx_eq */
			trace_step(depth, "eq:", NULL);
			compile(head->lefptr, tab, depth + 1);
			emit_result_to_arg();
			emit_call((uintptr_t)cx_eq_first);
			compile(head->lefptr ? head->lefptr->lefptr : NULL, tab, depth + 1);
			emit_result_to_arg();
			emit_call((uintptr_t)cx_eq_second);
			return;
		case SP_AND:
		case SP_OR:
			if (compile_andor(head, pe->special == SP_AND, tab, depth)) {
				return;
			}
			/* too many arguments: call lx_and / lx_or with the form */
			snprintf(what, sizeof what, "call lx_%s:", pe->name);
			trace_step(depth, what, x);
			emit_const_arg((uintptr_t)head);
			emit_call((uintptr_t)pe->fn);
			return;
		case SP_SQRT:
			if (isnullcell(head->lefptr)) {
				/* no argument: lx_plus reports it, evaluating nothing */
				trace_step(depth, "call lx_plus, param 4:", x);
				emit_const_arg((uintptr_t)head);
				emit_param(1, SQRT);
				emit_call((uintptr_t)lx_plus);
				return;
			}
			trace_step(depth, "sqrt:", NULL);
			compile(head->lefptr, tab, depth + 1); /* only the first arg is evaluated */
			emit_result_to_arg();
			emit_const_arg2((uintptr_t)head);
			emit_call((uintptr_t)cx_sqrt);
			return;
		case SP_COND:
			if (compile_cond(head, tab, depth)) {
				return;
			}
			/* a clause is not a list: let lx_cond report it */
			trace_step(depth, "call lx_cond:", x);
			emit_const_arg((uintptr_t)head);
			emit_call((uintptr_t)lx_cond);
			return;
		case SP_UNEVALARG:
			snprintf(what, sizeof what, "call lx_%s, unevaluated:", pe->name);
			trace_step(depth, what, head->lefptr);
			emit_const_arg((uintptr_t)head->lefptr);
			emit_call((uintptr_t)pe->fn);
			return;
		case SP_NONE:
			if (pe->fn == NULL) {
				break;
			}
			if (pe->evalarg) {
				compile(head->lefptr, tab, depth + 1); /* argument first */
				snprintf(what, sizeof what, "call lx_%s on the value", pe->name);
				trace_step(depth, what, NULL);
				emit_result_to_arg();
			} else {
				if (pe->nparams == 2) {
					snprintf(what, sizeof what, "call lx_%s, params %d %d:",
						pe->name, pe->paramval, pe->paramval2);
				} else if (pe->nparams == 1) {
					snprintf(what, sizeof what, "call lx_%s, param %d:", pe->name, pe->paramval);
				} else {
					snprintf(what, sizeof what, "call lx_%s:", pe->name);
				}
				trace_step(depth, what, x);
				emit_const_arg((uintptr_t)head);
				if (pe->nparams >= 1) {
					emit_param(1, pe->paramval);
				}
				if (pe->nparams == 2) {
					emit_param(2, pe->paramval2);
				}
			}
			emit_call((uintptr_t)pe->fn);
			return;
		default:
			break; /* SP_FALLBACK */
		}
	}
}
/* a call of a function defined by defun (including a redefined */
/* primitive name): call its compiled body, compiling it if new */
if (x->lstat == LSLST && x->r.rigptr != NULL && x->r.rigptr->lstat == IDATOM) {
	SLC *lam = lambda_of(x->r.rigptr);
	cfunc *cf;

	if (lam != NULL && (cf = cfunc_for(x->r.rigptr->r.idval, lam)) != NULL) {
		int nargs = compiled_arg_count(x, cf);

		if (nargs < 0) {
			/* args bound by lambda_bind / bind_uneval, as the interpreter */
			snprintf(what, sizeof what, "call compiled %s:", getident(cf->atomid));
			trace_step(depth, what, x);
			emit_const_arg((uintptr_t)x);
			emit_const_arg2((uintptr_t)cf);
			emit_call((uintptr_t)cx_call);
			return;
		}
		/* args compiled: each value bound pending, then the call */
		SLC *formal = (lam->r.rigptr)->lefptr->r.rigptr;
		SLC *arg = x->r.rigptr->lefptr;
		int i;

		snprintf(what, sizeof what, "call compiled %s, args compiled:", getident(cf->atomid));
		trace_step(depth, what, x);
		emit_call((uintptr_t)cx_frame_begin);
		for (i = 0; i < nargs; i++, arg = arg->lefptr, formal = formal->lefptr) {
			compile(arg, tab, depth + 1);
			emit_result_to_arg();
			emit_const_arg2((uintptr_t)formal);
			emit_call((uintptr_t)cx_frame_arg);
		}
		emit_const_arg((uintptr_t)cf);
		emit_call((uintptr_t)cx_frame_call);
		return;
	}
}
/* anything else: an uncompiled primitive, lambda, a variable, */
/* a number or null - let the interpreter do it */
trace_step(depth, "call lx_eval:", x);
emit_const_arg((uintptr_t)x);
emit_call((uintptr_t)lx_eval);
} /* end function compile */

/* compiles the bodies of all registered functions into the arena, */
/* one after another (compiling a body may register more). Returns */
/* false if the arena is full; the failed entries are removed again */
static bool compile_pending(primentry *tab)
{
cfunc *cf;
SLC *bodyform;
byte *start;
char what[120];

if (arena == NULL && pendingfns != NULL) {
	byte *m = mmap(NULL, ARENA_SIZE, PROT_READ | PROT_WRITE | PROT_EXEC,
			MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (m == MAP_FAILED) {
		puts("Fatal: compex: No executable memory allocate for functions");
		exit(8);
	}
	arena = m;
}
while ((cf = pendingfns) != NULL) {
	pendingfns = cf->nextpending;
	cf->nextpending = NULL;
	start = emit_p = arena + arena_used;
	emit_end = arena + ARENA_SIZE;
	emit_ok = true;
	snprintf(what, sizeof what, "compiling function %s:", getident(cf->atomid));
	trace_step(1, what, NULL);
	emit_prologue();
	/* (lambda args form1 form2...): the value is that of the last form */
	bodyform = (cf->lambda->r.rigptr)->lefptr->lefptr;
	if (bodyform == NULL) {
		compile(NULL, tab, 2);
	}
	for (; bodyform != NULL; bodyform = bodyform->lefptr) {
		compile(bodyform, tab, 2);
	}
	emit_epilogue();
	if (!emit_ok) {
		/* arena full: forget this and all waiting functions */
		ctable[cf->atomid] = NULL;
		free(cf);
		while ((cf = pendingfns) != NULL) {
			pendingfns = cf->nextpending;
			ctable[cf->atomid] = NULL;
			free(cf);
		}
		return false;
	}
	__builtin___clear_cache((char *)start, (char *)emit_p);
	cf->code = start;
	cf->len = (size_t)(emit_p - start);
	arena_used += cf->len;
	if (cx_print) {
		sprintf(outbuf, "  %s compiled at %p, %zu bytes\n",
			getident(cf->atomid), (void *)cf->code, cf->len);
		condpr(stdout);
	}
}
return true;
} /* end function compile_pending */
#endif



/* ---- the primitive table ---- */
/* id, then: name, function, evaluate arg first, number of extra params, */
/* param values, special handling; as the switch in lx_eval_internal. */
/* Unlisted ids stay zero, so they fall back to lx_eval(form). Filled on */
/* first use, by lx_compex or compex_lambda_call */
/* WARNING: this table must match the primitive switch in */
/* lx_eval_internal in main.c, and primindex in liststor.c: the same */
/* primitive at the same number, taking the same parameters, in the */
/* same order. An edit to a primitive in the switch (added, removed, */
/* renumbered, or its parameters changed) needs the matching edit here. */
static primentry ptable[80];       /* indexed by primitive id */
static bool goodtable = false;     /* true once ptable has been filled */

static void fill_table(void)
{
if (goodtable) {
	return;
}
	ptable[1]  = (primentry){"quote",   NULL,                 false, 0, 0, 0, SP_QUOTE};
	ptable[2]  = (primentry){"true",    (anyfn)lx_true,       false, 0, 0, 0, SP_NONE};
	ptable[3]  = (primentry){"eval",    (anyfn)lx_eval,       true,  0, 0, 0, SP_EVAL};
	ptable[4]  = (primentry){"lambda",  NULL,                 false, 0, 0, 0, SP_FALLBACK};
	ptable[5]  = (primentry){"cdr",     (anyfn)lx_cdr,        false, 0, 0, 0, SP_NONE};
	ptable[6]  = (primentry){"car",     (anyfn)lx_car,        false, 0, 0, 0, SP_NONE};
	ptable[7]  = (primentry){"cons",    (anyfn)lx_cons,       false, 0, 0, 0, SP_NONE};
	ptable[8]  = (primentry){"and",     (anyfn)lx_and,        false, 0, 0, 0, SP_AND};
	ptable[9]  = (primentry){"or",      (anyfn)lx_or,         false, 0, 0, 0, SP_OR};
	ptable[10] = (primentry){"cond",    (anyfn)lx_cond,       false, 0, 0, 0, SP_COND};
	ptable[11] = (primentry){"list",    (anyfn)lx_list,       false, 0, 0, 0, SP_NONE};
	ptable[12] = (primentry){"loop",    (anyfn)lx_loop,       false, 0, 0, 0, SP_NONE};
	ptable[13] = (primentry){"while",   (anyfn)lx_while,      false, 1, TRUE, 0, SP_NONE};  /* while */
	ptable[14] = (primentry){"while",   (anyfn)lx_while,      false, 1, FALSE, 0, SP_NONE}; /* until */
	ptable[15] = (primentry){"set",     (anyfn)lx_set,        false, 1, EVAL, 0, SP_NONE};  /* set */
	ptable[16] = (primentry){"set",     (anyfn)lx_set,        false, 1, NOEVAL, 0, SP_NONE}; /* setq */
	ptable[17] = (primentry){"eof",     (anyfn)lx_eof,        true,  0, 0, 0, SP_NONE};
	ptable[18] = (primentry){"ordinal", (anyfn)lx_ordinal,    true,  0, 0, 0, SP_NONE};
	ptable[19] = (primentry){"minusp",  (anyfn)lx_minusp,     true,  0, 0, 0, SP_NONE};
	ptable[20] = (primentry){"system",  (anyfn)lx_system,     true,  0, 0, 0, SP_RETARG};
	ptable[21] = (primentry){"plus",    (anyfn)lx_plus,       false, 1, PLUS, 0, SP_ARITH};       /* + */
	ptable[22] = (primentry){"plus",    (anyfn)lx_plus,       false, 1, TIMES, 0, SP_ARITH};      /* * */
	ptable[23] = (primentry){"plus",    (anyfn)lx_plus,       false, 1, DIFFERENCE, 0, SP_ARITH}; /* - */
	ptable[24] = (primentry){"plus",    (anyfn)lx_plus,       false, 1, DIVIDE, 0, SP_ARITH};     /* / */
	ptable[25] = (primentry){"plus",    (anyfn)lx_plus,       false, 1, SQRT, 0, SP_SQRT};       /* sqrt */
	ptable[26] = (primentry){"listp",   (anyfn)lx_listp,      true,  0, 0, 0, SP_NONE};
	ptable[27] = (primentry){"numberp", (anyfn)lx_numberp,    true,  0, 0, 0, SP_NONE};
	ptable[28] = (primentry){"atom",    (anyfn)lx_atom,       true,  0, 0, 0, SP_NONE};
	ptable[29] = (primentry){"null",    (anyfn)lx_null,       true,  0, 0, 0, SP_NONE}; /* null */
	ptable[30] = (primentry){"null",    (anyfn)lx_null,       true,  0, 0, 0, SP_NONE}; /* not */
	ptable[31] = (primentry){"length",  (anyfn)lx_length,     true,  0, 0, 0, SP_NONE};
	ptable[32] = (primentry){"obl",     (anyfn)lx_obl,        true,  0, 0, 0, SP_NONE};
	ptable[33] = (primentry){"print",   NULL,                 true,  2, SPACE, NOESC, SP_PRINT};
	ptable[34] = (primentry){"prin",    NULL,                 true,  2, NOSPACE, NOESC, SP_PRINT};
	ptable[35] = (primentry){"princ",   NULL,                 true,  2, NOSPACE, ESC, SP_PRINT};
	ptable[36] = (primentry){"load",    (anyfn)lx_load,       true,  0, 0, 0, SP_RETARG};
	ptable[37] = (primentry){"readch",  (anyfn)lx_readch,     true,  0, 0, 0, SP_NONE};
	ptable[38] = (primentry){"explode", (anyfn)lx_explode,    true,  0, 0, 0, SP_NONE};
	ptable[39] = (primentry){"append",  (anyfn)lx_append,     false, 0, 0, 0, SP_NONE};
	ptable[40] = (primentry){"read",    (anyfn)lx_read,       true,  0, 0, 0, SP_NONE};
	ptable[41] = (primentry){"open",    (anyfn)lx_open,       false, 0, 0, 0, SP_NONE};
	ptable[42] = (primentry){"close",   (anyfn)lx_close,      true,  0, 0, 0, SP_NONE};
	ptable[43] = (primentry){"put",     (anyfn)lx_put,        false, 0, 0, 0, SP_NONE};
	ptable[44] = (primentry){"remprop", (anyfn)lx_remprop,    false, 0, 0, 0, SP_NONE};
	ptable[45] = (primentry){"get",     (anyfn)lx_get,        false, 0, 0, 0, SP_NONE};
	ptable[46] = (primentry){"implode", (anyfn)lx_implode,    true,  0, 0, 0, SP_NONE};
	ptable[47] = (primentry){"rplaca",  (anyfn)lx_rplaca,     false, 0, 0, 0, SP_NONE};
	ptable[48] = (primentry){"rplacd",  (anyfn)lx_rplacd,     false, 0, 0, 0, SP_NONE};
	ptable[49] = (primentry){"write",   (anyfn)lx_write,      false, 2, SPACE, ESC, SP_NONE};     /* writec */
	ptable[50] = (primentry){"write",   (anyfn)lx_write,      false, 2, NOSPACE, NOESC, SP_NONE}; /* writen */
	ptable[51] = (primentry){"write",   (anyfn)lx_write,      false, 2, SPACE, NOESC, SP_NONE};   /* write */
	ptable[52] = (primentry){"reverse", (anyfn)lx_reverse,    true,  0, 0, 0, SP_NONE};
	ptable[53] = (primentry){"eq",      (anyfn)lx_eq,         false, 0, 0, 0, SP_EQ};
	ptable[54] = (primentry){"initturtle", (anyfn)lx_initturtle, false, 0, 0, 0, SP_NONE};
	ptable[55] = (primentry){"home",    (anyfn)lx_home,       false, 0, 0, 0, SP_NONE};
	ptable[56] = (primentry){"pendown", (anyfn)lx_pendown,    false, 0, 0, 0, SP_NONE};
	ptable[57] = (primentry){"setfill", (anyfn)lx_setfill,    false, 0, 0, 0, SP_NONE};
	ptable[58] = (primentry){"pencolour", (anyfn)lx_pencolour, false, 0, 0, 0, SP_NONE};
	ptable[59] = (primentry){"fillcolour", (anyfn)lx_fillcolour, false, 0, 0, 0, SP_NONE};
	ptable[60] = (primentry){"turn",    (anyfn)lx_turn,       false, 0, 0, 0, SP_NONE};
	ptable[61] = (primentry){"turnto",  (anyfn)lx_turnto,     false, 0, 0, 0, SP_NONE};
	ptable[62] = (primentry){"move",    (anyfn)lx_move,       false, 0, 0, 0, SP_NONE};
	ptable[63] = (primentry){"moveto",  (anyfn)lx_moveto,     false, 0, 0, 0, SP_NONE};
	ptable[64] = (primentry){"circle",  (anyfn)lx_circle,     false, 0, 0, 0, SP_NONE};
	ptable[65] = (primentry){"ellipse", (anyfn)lx_ellipse,    false, 0, 0, 0, SP_NONE};
	ptable[66] = (primentry){"rectangle", (anyfn)lx_rectangle, false, 0, 0, 0, SP_NONE};
	ptable[67] = (primentry){"onscreen", (anyfn)lx_onscreen,  false, 0, 0, 0, SP_NONE};
	ptable[68] = (primentry){"polygon", (anyfn)lx_polygon,    false, 0, 0, 0, SP_NONE};
	ptable[69] = (primentry){"let",     (anyfn)lx_let,        false, 0, 0, 0, SP_NONE};
	ptable[70] = (primentry){"letstar", (anyfn)lx_letstar,    false, 0, 0, 0, SP_NONE};
	ptable[71] = (primentry){"compex",  NULL,                 false, 0, 0, 0, SP_FALLBACK}; /* never compile compex itself */
	ptable[72] = (primentry){"defined", (anyfn)lx_defined,    false, 0, 0, 0, SP_UNEVALARG};
goodtable = true;
} /* end function fill_table */


/* ---- compex modes ---- */
/* (compex 0) interpreter only (the default), (compex 1) calls of */
/* defun'd functions run their compiled bodies (compiled on first use), */
/* (compex 2) both run and a warning is printed if the results differ. */
/* The interpreter calls compex_lambda_call (from lx_eval_internal) for */
/* a lambda call whenever compex_mode is not 0 */
int compex_mode = 0;
static bool comparing = false; /* mode 2 is running a function both ways */

#if CODE_SUPPORTED
/* structural equality of two values, as equal in init.lsp */
static bool cx_equal(SLC *a, SLC *b)
{
bool na = isnullcell(a), nb = isnullcell(b);
SLC *ea, *eb;

if (na || nb) {
	return na && nb;
}
if (a->lstat != b->lstat) {
	return false;
}
if (a->lstat == LSLST) {
	for (ea = a->r.rigptr, eb = b->r.rigptr; ea && eb; ea = ea->lefptr, eb = eb->lefptr) {
		if (!cx_equal(ea, eb)) {
			return false;
		}
	}
	return ea == NULL && eb == NULL;
}
if (a->lstat == NUMATOM) {
	if (a->isfptr || b->isfptr) {
		return a->isfptr == b->isfptr && a->r.rigfp == b->r.rigfp;
	}
	return a->r.rigval == b->r.rigval;
}
return a->r.idval == b->r.idval; /* IDATOM */
} /* end function cx_equal */

/* prints just this value, not the rest of a list it is in */
static void print_value(SLC *v)
{
SLC one;

if (v == NULL) {
	lx_prin(stdout, NULL, SPACE, NOESC);
	return;
}
one = *v;
one.lefptr = NULL;
lx_prin(stdout, &one, SPACE, NOESC);
} /* end function print_value */
#endif

/* called by lx_eval_internal (main.c) instead of do_lambda when */
/* compex_mode is not 0: inptr is the call, form the (lambda ...) found */
SLC *compex_lambda_call(SLC *inptr, SLC *form)
{
#if CODE_SUPPORTED
SLC *head = inptr->r.rigptr;
SLC *lam, *ri, *rc;
cfunc *cf;
int id;

/* form is lx_eval_internal's copy of the definition cell, so compare */
/* what they point to; the table keeps the real definition, lam */
if (comparing || head == NULL || head->lstat != IDATOM
    || (lam = lambda_of(head)) == NULL || lam->r.rigptr != form->r.rigptr) {
	/* comparing already, or an anonymous ((lambda ...) args) or an */
	/* alias: as the interpreter */
	return do_lambda(inptr, form);
}
id = head->r.idval;
fill_table();
cf = cfunc_for(id, lam);
if (cf == NULL) {
	return do_lambda(inptr, form);
}
if (cf->code == NULL) {
	cx_print = (getenv("LISPCSPRINT") != NULL);
	if (!compile_pending(ptable)) {
		return do_lambda(inptr, form); /* no room: interpret */
	}
}
if (compex_mode == 1) {
	return cx_call(inptr, cf);
}
/* mode 2: both, nested calls meanwhile only interpreted (else each */
/* level would run both again, exponentially) */
comparing = true;
ri = do_lambda(inptr, form);
mark_req(ri);
rc = cx_call(inptr, cf);
mark_not(ri);
comparing = false;
if (!cx_equal(ri, rc)) {
	sprintf(outbuf, "compex: %s: interpreted ", getident(id)); condpr(stdout);
	print_value(ri);
	sprintf(outbuf, ", compiled "); condpr(stdout);
	print_value(rc);
	sprintf(outbuf, "\n"); condpr(stdout);
}
return ri;
#else
return do_lambda(inptr, form);
#endif
} /* end function compex_lambda_call */

/* (bytes used, size) of the store for compiled function bodies */
static SLC *compex_status(void)
{
SLC *res, *n1, *n2;
float used = 0, size = 0;

#if CODE_SUPPORTED
used = (float)arena_used;
size = (float)ARENA_SIZE;
#endif
mark_req(res = getfree());
n1 = getfree();
res->r.rigptr = n1; /* reachable before the next getfree */
n1->lstat = NUMATOM;
n1->r.rigval = used;
n2 = getfree();
n1->lefptr = n2;
n2->lstat = NUMATOM;
n2->r.rigval = size;
mark_not(res);
return res;
} /* end function compex_status */



SLC *lx_compex(SLC *form)
{
/* (compex N), N 0, 1 or 2: sets the mode (see compex_lambda_call); */
/* (compex): leaves it. Both return (bytes-used store-size). */
/* (compex form), any other argument: compiles the form to machine */
/* code, runs that and returns its result. If LISPCSPRINT is set, it */
/* first evaluates the form with the interpreter too and prints both */
/* results and the compile trace; if not, only the compiled code runs */
static byte *memptr = NULL;        /* executable area, allocated once */
static bool running = false;       /* compiled code is running */
SLC *x, *retval;

x = form->lefptr;
if (x == NULL) {
	return compex_status();
}
if (x->lstat == NUMATOM && x->isfptr == 0) {
	if (x->r.rigval == 0 || x->r.rigval == 1 || x->r.rigval == 2) {
		compex_mode = (int)x->r.rigval;
		return compex_status();
	}
	return report_error("compex", "mode must be 0, 1 or 2", x, TRUE);
}

if (running) {
	/* compex called from inside compiled code: compiling now would */
	/* overwrite the code that is running, so just interpret */
	return lx_eval(x);
}

if (memptr == NULL) {
	byte *m = mmap(NULL, CODEMEM_SIZE, PROT_READ | PROT_WRITE | PROT_EXEC,
			MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (m == MAP_FAILED) {
		puts("Fatal: compex: No executable memory allocate");
		exit(8);
	}
	memptr = m;
}
fill_table();

cx_print = (getenv("LISPCSPRINT") != NULL);

if (cx_print) {
	retval = lx_eval(x); /* the interpreter's result, to compare */
	sprintf(outbuf, "interpreter: "); condpr(stdout);
	lx_prin(stdout, retval, SPACE, NOESC);
	sprintf(outbuf, "\n"); condpr(stdout);
}

#if CODE_SUPPORTED
{
SLC *(*codefn)(void);
emit_p = memptr;
emit_end = memptr + CODEMEM_SIZE;
emit_ok = true;
emit_prologue();
compile(x, ptable, 1);
emit_epilogue();
if (!emit_ok) {
	return report_error("compex", "form too big for the code area", x, TRUE);
}
__builtin___clear_cache((char *)memptr, (char *)emit_p); /* sync I-cache */
/* now the bodies of any user functions it calls, before anything runs */
if (!compile_pending(ptable)) {
	return report_error("compex", "no room left for compiled functions", x, TRUE);
}
memcpy(&codefn, &memptr, sizeof codefn); /* data pointer -> function pointer */
arith_depth = 0; /* in case an abort left compiled arithmetic unfinished */
frame_depth = 0; /* or a compiled call half set up */
eq_depth = 0;    /* or an eq */
running = true;
retval = codefn(); /* run the compiled code */
running = false;
if (cx_print) {
	sprintf(outbuf, "compiled:    "); condpr(stdout);
	lx_prin(stdout, retval, SPACE, NOESC);
	sprintf(outbuf, "\n"); condpr(stdout);
}
return retval;
}
#else
return report_error("compex", "code writing not supported on this CPU", NULL, FALSE);
#endif
} /* end function lx_compex */



