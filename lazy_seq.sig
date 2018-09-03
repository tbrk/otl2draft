(* $Id: lazy_seq.sig 74 2008-02-26 11:13:05Z tbourke $
 *
 * Extended from Paulson, Section 8.4.
 *
 * DISCLAIMER OF WARRANTY
 * The programs listed in this book are provided `as is' without warranty of any
 * kind. We make no warranties, express or implied, that the programs are free
 * of error, or are consistent with any particular standard of merchantability,
 * or that they will meet your requirements for any particular application. They
 * should not be relied upon for solving a problem whose incorrect solution
 * could result in injury to a person or loss of property. If you do use the
 * programs or procedures in such a manner, it is at your own risk. The author
 * and publisher disclaim all liability for direct, incidental or consequential
 * damages resulting from your use of the programs, modules or functions in this
 * book.
 *
 * Not completely lazy (the next value is prepared before it is requested).
 *)
signature LAZY_SEQ =
sig
    type 'a t
    exception Empty
    val empty       : 'a t
    val cons        : 'a * (unit -> 'a t) -> 'a t
    val null        : 'a t -> bool
    val hd          : 'a t -> 'a
    val tl          : 'a t -> 'a t
    val take        : 'a t * int -> 'a list
    val toList      : 'a t -> 'a list
    val fromList    : 'a list -> 'a t
    val @           : 'a t * 'a t -> 'a t
    val interleave  : 'a t * 'a t -> 'a t
    val concat      : 'a t t -> 'a t
    val map         : ('a -> 'b) -> 'a t -> 'b t
    val filter      : ('a -> bool) -> 'a t -> 'a t
    val cycle       : ((unit -> 'a t) -> 'a t) -> 'a t

    val generate    : (unit -> 'a option) -> 'a t
end;
