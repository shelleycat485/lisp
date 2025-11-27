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
 #include <signal.h>
#include <setjmp.h>
#include "listspec.h"
#include <stdlib.h> 

/* This is based on the example signal.c */
/* it sets up a control C handler that returns to a known point */
/* in the program     */

void ctrlchandler(int sig);          /* Prototypes */

void set_control_c ()
{
  
    /* Modify CTRL+C behavior. */
    if( signal(  SIGINT , ctrlchandler ) == SIG_ERR )
    {
	fprintf( stderr, "Couldn't set SIGINT\n" );
	exit(7);
    }  
} /* end function set_control_c */

/* Handles SIGINT (CTRL+C) interrupt. */
void ctrlchandler(int sig)
{

    /* Disallow CTRL+C during handler. */
	signal( SIGINT, SIG_IGN );

	/* The CTRL+C interrupt must be reset to our handler since by
	 * default it is reset to the system handler.
	 */
	signal( SIGINT, ctrlchandler );
	longjmp (main_env , 1);  /* 1 signifes break in */

}

void check_keyboard(void)
{
      ;
/* this checks the shift status and does the longjmp if */
/* control and shift are pressed together               */

} /* end function check_keyboard */

