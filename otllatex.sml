(* $Id: otllatex.sml 138 2008-04-28 04:21:34Z tbourke $
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
structure OtlLatex : sig
                       val go : {topLevel    : int,
                                 foldMarks   : bool,
                                 convertChars: bool}
                                * TextIO.StreamIO.instream
                                * TextIO.outstream -> unit
                     end =
struct

  fun writeTrees ({topLevel, foldMarks, convertChars}, tlist, os) = let
      fun pr s = TextIO.output (os, s)

      fun prConvert s = let
          fun exf #"\\" = "$\\backslash$"
            | exf #"#"  = "\\#"
            | exf #"$"  = "\\$"
            | exf #"%"  = "\\%"
            | exf #"^"  = "\\^{}"
            | exf #"&"  = "\\&"
            | exf #"_"  = "\\_"
            | exf #"{"  = "\\{"
            | exf #"}"  = "\\}"
            | exf #"~"  = "\\~{}"
            | exf c     = String.str c
        in
          TextIO.output (os, (Substring.translate exf (Substring.full s)))
        end

      val prt = if convertChars then prConvert else pr

      val maxHead = 4 - topLevel

      fun doHead ( 0, n) = (pr "\\part{"; prt n; pr "}")
        | doHead ( 1, n) = (pr "\\chapter{"; prt n; pr "}")
        | doHead ( 2, n) = (pr "\\section{"; prt n; pr "}")
        | doHead ( 3, n) = (pr "\\subsection{"; prt n; pr "}")
        | doHead ( 4, n) = (pr "\\subsubsection{"; prt n; pr "}")
        | doHead _       = raise Fail "internal error: maxHead not respected."

      fun showHead (l, n) = doHead (l + topLevel, n)
      
      fun foldLevel l = if topLevel > 2 then l + (topLevel - 1) else l + 1

      val prOFold = if foldMarks
                    then (fn l=>pr (" %{{{" ^ Int.toString (foldLevel l)))
                    else (fn _=>())
      local
        fun cl 1 = "%--   }}}1%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
          | cl 2 = "%---  }}}2------------------------------------------------------------\n"
          | cl 3 = "%---- }}}3- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"
          | cl n = "%-----}}}" ^ Int.toString n ^ "\n"
      in
      val prCFold = if foldMarks then (fn l=>pr (cl (foldLevel l))) else (fn _=>())
      end (* local *)

      fun showList lty is = (pr "\\begin{"; pr lty; pr "}\n";
                             List.app (fn i=>(pr "\\item "; prt i)) is;
                             pr "\\end{"; pr lty; pr "}\n")
      val showAList = showList "itemize"
      val showDList = showList "enumerate"

      fun fblock (clvl, []) = []
        | fblock (clvl, cs as (OtlTree.Section {level, name, contents}::xs)) =
          if level < clvl then cs
          else (pr "\\item["; prt name; pr "]\n"; f contents; fblock (clvl, xs))
        | fblock (clvl, OtlTree.Text t::xs)  = (pr t; fblock (clvl, xs))
        | fblock (clvl, OtlTree.AList is::xs) = (showAList is; fblock (clvl,xs))
        | fblock (clvl, OtlTree.DList is::xs) = (showDList is; fblock (clvl,xs))

      and f [] = ()
        | f (cs as (OtlTree.Section {level, name, contents}::xs)) =
          if level <= maxHead
          then (showHead (level, name); prOFold level; pr "\n";
                f contents; prCFold level;
                f xs)
          else let
                 val _ = pr "\n\\begin{description}\n"
                 val cs' = fblock (level, cs)
                 val _ = pr "\\end{description}\n"
               in f cs' end

        | f (OtlTree.Text t::xs)  = (prt t; f xs)
        | f (OtlTree.AList is::xs) = (showAList is; f xs)
        | f (OtlTree.DList is::xs) = (showDList is; f xs)
    in f tlist end

  fun go (options, is, os) = let
      val scan = OtlLine.scan TextIO.StreamIO.inputLine
      val trees = OtlTree.make (OtlReader.scanSeq (scan,
                                  TextIO.StreamIO.closeIn) is)
    in writeTrees (options, trees, os) end;

end
