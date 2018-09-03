(* $Id: otl_reader.sig 77 2008-02-28 03:01:40Z tbourke $
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
signature OTL_READER =
sig
  datatype bullet = Asterisk | Dash

  datatype t = Heading of int * string * bool  (* true if marked '+' *)
             | Text    of int * string list
             | List    of int * string list list * bullet

  val toString : t -> string
  val level    : t -> int

  val scan     : (OtlLine.t, 'a) StringCvt.reader -> (t, 'a) StringCvt.reader

  val scanSeq  : (OtlLine.t, 'a) StringCvt.reader * ('a -> unit)
                  -> 'a -> t LazySeq.t
    (* The second function is called when the reader returns NONE.
     * It's intended for closing the stream. Could we just rely on garbage
     * collection? *)

end;
