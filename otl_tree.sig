(* $Id: otl_tree.sig 138 2008-04-28 04:21:34Z tbourke $
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
signature OTL_TREE =
sig
  datatype t = Section of {level: int, name: string, contents: t list}
             | Text of string
             | AList of string list (* asterisk *)
             | DList of string list (* dash     *)

  val make : OtlReader.t LazySeq.t -> t list
    (* NB: quietly removes bad level skips, e.g. (_ = tab)
     *        heading1            heading1
     *        | text 1            | text 1
     *        __| text 2   ===>   | text 2
     *        ___| text 3         | text 3
     *        | text 4            | text 4                   *)
end
