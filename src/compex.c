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
extern SLC *lx_compex		    (SLC *); /* for testing complilations */
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



SLC *lx_compex(SLC *form)
{
  //SLC *res;
  //res = lx_eval(form->lefptr);
  //lx_prin(stdout, res, SPACE, NOESC); 

  // this works return lx_cdr(form);
  // this works return lx_car(form);
  return lx_or(form);
} /* end function lx_compex */



