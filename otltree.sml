(* $Id: otltree.sml 138 2008-04-28 04:21:34Z tbourke $
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
structure OtlTree : OTL_TREE =
struct
  structure O = OtlReader

  datatype t = Section of {level: int, name: string, contents: t list}
             | Text of string
             | AList of string list
             | DList of string list

  fun make seq = let
      
      fun makeList (O.Asterisk, lss) = (AList (map concat lss))
        | makeList (O.Dash,     lss) = (DList (map concat lss))
      
      fun r (clvl, seq, cs) = let
          fun next v  = r (clvl, LazySeq.tl seq, v::cs)
          fun done () = (rev cs, seq)
        in if LazySeq.null seq then done ()
           else case LazySeq.hd seq of
                  (O.Heading (l, nm, _)) =>
                      if l <= clvl then done ()
                      else let val (ncs, seq') = r (l, LazySeq.tl seq, [])
                               val v = Section {level=l, name=nm, contents=ncs}
                           in r (clvl, seq', v::cs) end

                | (O.Text (l, ts))  => if l < clvl then done ()
                                       else next (Text (String.concat ts))
                | (O.List (l, lss, b)) => if l < clvl then done ()
                                          else next (makeList (b, lss))
        end

    in #1 (r (~1, seq, [])) end
end
