(* $Id: otl_line.sig 76 2008-02-28 02:59:15Z tbourke $
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
signature OTL_LINE =
sig
  datatype bullet_type = Asterisk
                       | Dash

  datatype t = Heading of int * substring * bool
             | Text    of ({level  : int,
                            indent : int,
                            value  : substring})
             | Bullet  of ({level  : int,
                            indent : int,
                            value  : substring}) * bullet_type

  val scan      : (string, 'a) StringCvt.reader -> (t, 'a) StringCvt.reader
    (* the input reader must provide complete otl lines.
     * TEXT_STREAM_IO.inputLine from an otl file is suitable. *)
end
