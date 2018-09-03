(* $Id: otlreader.sml 77 2008-02-28 03:01:40Z tbourke $
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
structure OtlReader : OTL_READER =
struct
  datatype bullet = Asterisk | Dash

  datatype t = Heading of int * string * bool
             | Text    of int * string list
             | List    of int * string list list * bullet

  fun level (Heading (i, _, _)) = i
    | level (Text (i, _))       = i
    | level (List (i, _, _))    = i

  local
    fun ltos l = Int.toString l ^ ") "
    val join  = String.concatWith "   "
    fun itos Asterisk i = "* " ^ String.concatWith "     " i
      | itos Dash i     = "- " ^ String.concatWith "     " i
  in
  fun toString (Heading (l, h, m)) = ltos l ^ h ^ (if m then " (+)" else "")
    | toString (Text (l, ts))      = ltos l ^ join ts
    | toString (List (l, is, b))   = ltos l ^ join (map (itos b) is)
  end

  structure SS = Substring
        and OL = OtlLine

  fun dropNL ss = SS.dropr (fn c=> c= #"\n") ss

  fun dropSpace ss = case SS.getc ss of
                       SOME (#" ", ss') => SS.string ss'
                     | _ => SS.string ss

  fun scan getLine s = let
      fun readText (l, xs, s) = case getLine s of
              SOME (OL.Text {level, value, ...}, s') =>
                  if level = l then readText (l, dropSpace value::xs, s')
                               else (Text (l, rev xs), s)
            | _                  => (Text (l, rev xs), s)

      fun readList (l, i, bullet) (fl, s) = let
          val b = case bullet of OL.Asterisk => Asterisk
                               | OL.Dash     => Dash

          fun dropIndent ss = dropSpace (SS.triml (i + 1) ss)
          fun makeItem xs = rev xs
          fun makeList xs = List (l, rev xs, b)

          fun f (xs, items, s) = case getLine s of
              SOME (OL.Text {level, indent, value}, s') =>
                  if level = l andalso indent > i
                  then f (dropIndent value::xs, items, s')
                  else (makeList (makeItem xs::items), s)
            | SOME (OL.Bullet ({level, value, ...},_), s') =>
                  if level = l
                  then f ([dropIndent value], makeItem xs::items, s')
                  else (makeList (makeItem xs::items), s)
            | _ => (makeList (makeItem xs::items), s)
          
        in f ([dropIndent fl], [], s) end

      fun read (OL.Heading (l,h,m), s) =
                  (Heading (l, SS.string (dropNL h), m), s)
        | read (OL.Text {level,indent,value}, s) =
                  readText (level, [dropSpace value], s)
        | read (OL.Bullet ({level,indent,value}, b), s) =
                  readList (level, indent, b) (value, s)
    in Option.map read (getLine s) end

  fun scanSeq (getLine, close) s = let
      val st = ref {strm=s, cLevel=0, line=1, errors=[]}

      fun checkLine ({strm, cLevel, line, errors}) = let
          fun warn s = (concat [Int.toString line, ":", s])::errors
          fun chkContent lvl = if (cLevel = lvl) then errors
                               else if (lvl > cLevel)
                                    then warn "No heading for content (>)"
                                    else warn "No heading for content (<)"

          fun procVal (v, strm') = let
              val (l, errors') = case v of
                  OL.Heading (level, _, _)=> if (level <= cLevel + 1)
                                             then (level, errors)
                                             else (level,
                                                   warn "Missing heading level")
                | OL.Text {level,...}      => (level, chkContent level)
                | OL.Bullet ({level,...},_)=> (level, chkContent level)
            in (v, {strm=strm', cLevel=l, line=line+1, errors=errors'}) end

        in Option.map procVal (getLine strm) end

      fun r () = case scan checkLine (!st) of
                   NONE          => (close (#strm (!st));
                                     List.app (fn s=>(TextIO.output
                                                        (TextIO.stdErr, s);
                                                      TextIO.output
                                                        (TextIO.stdErr, "\n")))
                                              (rev (#errors (!st)));
                                     NONE)
                 | SOME (v, st') => (st := st'; SOME v)

    in LazySeq.generate r end

end
