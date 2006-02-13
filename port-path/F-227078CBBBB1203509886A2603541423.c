/* Copyright 2005 Hannes Mehnert, Florian Lorenzen, Fabian Otto
 *
 * This file is part of port-path.
 *
 * port-path is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * port-path is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with gennf; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * $Id: F-227078CBBBB1203509886A2603541423.c,v 1.1 2006/02/13 17:21:07 florenz Exp $
 */

/* Glue code to call some POSIX functions from Lisp. */

#include <stdio.h>

char *port_path_tempnam() {
  return tempnam((void *)0, (void *)0);
}
