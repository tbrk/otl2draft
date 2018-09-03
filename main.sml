(* $Id: main.sml 139 2008-04-28 06:31:00Z tbourke $
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

(* TODO: - add exception handling and reasonable error messages. *)
structure Main :
sig
  val progName : string
  val version  : string

  val callmain : string * string list -> OS.Process.status
  val main     : unit -> unit
end
 =
struct
  val progName = "otl2draft"
  val version  = "1.0.0"

  exception SilentError

  datatype args = ShowUsage
                | ShowVersion
                | TopLevel of int
                | InputFile of string
                | OutputFile of string
                | FoldMarks
                | AddPrelude
                | ConvertChars

  val sectionLevel = 2
  val settings = {inputfile=ref (NONE : string option),
                  outputfile=ref (NONE : string option),
                  topLevel=ref sectionLevel,
                  foldMarks=ref false,
                  addPrelude=ref false,
                  convertChars=ref false,
                  infoShown=ref false}

  structure GO = GetOpt

  val optionList = [
      {short="h", long=["help"], desc=GO.NoArg (fn ()=>ShowUsage),
       help="Show this summary of command line options."},

      {short="v", long=["version"],
       desc=GO.NoArg (fn ()=>ShowVersion),
       help="Show the version number."},

      {short="f", long=["foldmarks"],
       desc=GO.NoArg (fn ()=>FoldMarks),
       help="Add foldmark comments to section headers."},

      {short="p", long=["prelude"],
       desc=GO.NoArg (fn ()=>AddPrelude),
       help="Add a simple LaTeX prelude."},

      {short="c", long=["convert"],
       desc=GO.NoArg (fn ()=>ConvertChars),
       help="Convert reserved LaTeX characters (#, $, %, ^, &, _, {, }, ~, \\)."},

      {short="", long=["part"],
       desc=GO.NoArg (fn ()=>TopLevel 0),
       help="Use \\part for topmost headings."},

      {short="", long=["chapter"],
       desc=GO.NoArg (fn ()=>TopLevel 1),
       help="Use \\chapter for topmost headings."},

      {short="", long=["section"],
       desc=GO.NoArg (fn ()=>TopLevel 2),
       help="Use \\section for topmost headings (the default)."},

      {short="", long=["subsection"],
       desc=GO.NoArg (fn ()=>TopLevel 3),
       help="Use \\subsection for topmost headings."},

      {short="", long=["subsubsection"],
       desc=GO.NoArg (fn ()=>TopLevel 4),
       help="Use \\subsubsection for topmost headings."},

      {short="i", long=["input"],
       desc=GO.ReqArg (InputFile, "path"),
       help="Otl file to use as input."},

      {short="o", long=["output"],
       desc=GO.ReqArg (OutputFile, "path"),
       help="Path to file which should be overwritten with results."}

    ]

  fun processOption ShowUsage      = (TextIO.print (GO.usageInfo
                                        {header=(progName^" "^version),
                                         options=optionList});
                                      TextIO.print "\n";
                                      (#infoShown settings) := true)
    | processOption ShowVersion    = (TextIO.print (version^"\n");
                                      (#infoShown settings) := true)
    | processOption (InputFile s)  = ((#inputfile settings) := SOME s)
    | processOption (OutputFile s) = ((#outputfile settings) := SOME s)
    | processOption (TopLevel i)   = ((#topLevel settings) := i)
    | processOption FoldMarks      = ((#foldMarks settings) := true)
    | processOption AddPrelude     = ((#addPrelude settings) := true)
    | processOption ConvertChars   = ((#convertChars settings) := true)

  fun wrapFiles []       = []
    | wrapFiles (f::fs)  = InputFile f::wrapFiles' fs
  and wrapFiles' []      = []
    | wrapFiles' (f::fs) = OutputFile f::wrapFiles fs

  fun showError s = TextIO.output (TextIO.stdErr, s)

  fun printPrelude os = let in
      if !(#topLevel settings) < sectionLevel
      then TextIO.output (os, "\\documentclass{book}\n")
      else TextIO.output (os, "\\documentclass{article}\n");
      TextIO.output (os, "\\begin{document}\n")
    end

  fun fileError (SOME f, causeexn) = (TextIO.output (TextIO.stdErr, concat [
                                          "error accessing ", f, " (",
                                          General.exnMessage causeexn, ")\n"]);
                                      raise SilentError)
    | fileError (NONE, causeexn) = raise causeexn

  fun callmain (name, args) = let
      val (ops, files) = GO.getOpt {argOrder=GO.Permute,
                                    options=optionList,
                                    errFn=showError} args
      val options = app processOption (ops @ wrapFiles files)

      val is = TextIO.getInstream (
                 Option.getOpt (Option.map TextIO.openIn
                                  (!(#inputfile settings)), TextIO.stdIn))
         handle IO.Io {cause, ...} => fileError (!(#inputfile settings), cause)

      val os = Option.getOpt (Option.map TextIO.openOut
                                  (!(#outputfile settings)), TextIO.stdOut)
         handle IO.Io {cause, ...} => fileError (!(#outputfile settings), cause)
    in
      if !(#addPrelude settings) then printPrelude os else ();

      if isSome (!(#inputfile settings))  orelse
         isSome (!(#outputfile settings)) orelse
         not (!(#infoShown settings))
      then OtlLatex.go ({topLevel= !(#topLevel settings),
                         foldMarks= !(#foldMarks settings),
                         convertChars= !(#convertChars settings)}, is, os)
      else ();

      if !(#addPrelude settings) then TextIO.output (os, "\\end{document}\n")
      else ();

      if isSome (!(#inputfile settings)) then TextIO.StreamIO.closeIn is
                                         else ();
      if isSome (!(#outputfile settings)) then TextIO.closeOut os
                                          else ();

      OS.Process.success
    end
    handle Fail s => (TextIO.output (TextIO.stdErr, s ^ "\n");
                      OS.Process.failure)
         | IO.Io {cause, ...} => (TextIO.output (TextIO.stdErr,
                                 (General.exnMessage cause) ^ "\n");
                                 OS.Process.failure)
         | SilentError => OS.Process.failure

    fun main () = ignore (callmain (CommandLine.name (),
                                    CommandLine.arguments ()));

end
