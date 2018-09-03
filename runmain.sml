(* $Id: runmain.sml 138 2008-04-28 04:21:34Z tbourke $
 *
 * Copyright (c) 2008 Timothy Bourke. All rights reserved.
 * 
 * This program is free software; you can redistribute it and/or 
 * modify it under the terms of the "BSD License" which is 
 * distributed with the software in the file LICENSE.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the BSD
 * License for more details.
 *
 *)
val _ = Main.callmain (CommandLine.name (), CommandLine.arguments ());

