(* $Id: otlline.sml 138 2008-04-28 04:21:34Z tbourke $
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
structure OtlLine : OTL_LINE =
struct
  datatype bullet_type = Asterisk
                       | Dash

  datatype t = Heading of int * substring * bool
             | Text    of detail
             | Bullet  of detail * bullet_type
    withtype detail = ({level  : int,
                        indent : int,
                        value  : substring})

  structure SS = Substring
  
  fun isBullet ss = case SS.first ss of
                      SOME #"*" => true
                    | SOME #"-" => true
                    | _         => false

  
  fun parse s = let
      val (tabs, v) = SS.splitl (fn c=> c= #"\t") (SS.full s)
      val lvl = SS.size tabs
    in
      case SS.getc (SS.dropl Char.isSpace v) of
        NONE            => Text {level=lvl, indent=0, value=v}
      | SOME (#"|", v') => let val (ws, t) = SS.splitl Char.isSpace v'
                               val d = {level=lvl, indent=SS.size ws, value=v'}
                           in case SS.first t of
                                SOME #"*" => Bullet (d, Asterisk)
                              | SOME #"-" => Bullet (d, Dash)
                              | _         => Text d
                           end
      | SOME (#"+", v)  => Heading (lvl, SS.dropl Char.isSpace v, true)
      | _               => Heading (lvl, v, false)
    end

  fun scan getLine s = Option.mapPartial (fn (l,s')=>SOME (parse l, s'))
                                         (getLine s)
    (* better to be lazy, i.e. cache parse results, but this complicates the
     * use of the reader pattern. *)

end
