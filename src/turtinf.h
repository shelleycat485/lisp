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



SLC *lx_initturtle                  (SLC *form);
SLC *lx_home                        (SLC *form);
SLC *lx_pendown                     (SLC *form);
SLC *lx_setfill                     (SLC *form);
SLC *lx_pencolour                   (SLC *form);
SLC *lx_fillcolour                  (SLC *form);
SLC *lx_turn                        (SLC *form);
SLC *lx_turnto                      (SLC *form);
SLC *lx_move                        (SLC *form);
SLC *lx_moveto                      (SLC *form);
SLC *lx_circle                      (SLC *form);
SLC *lx_ellipse                     (SLC *form);
SLC *lx_rectangle                   (SLC *form);
SLC *lx_onscreen                    (SLC *form);
SLC *lx_polygon                     (SLC *form);
