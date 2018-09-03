(* $Id: lazyseq.sml 74 2008-02-26 11:13:05Z tbourke $
 *
 * Extended from Paulson, Section 8.4
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
 *)
structure LazySeq :> LAZY_SEQ =
struct
    datatype 'a t = Nil
                  | Cons of 'a * ('a t) ref
                  | Delayed of unit -> 'a t

    exception Empty

    fun delay xf = ref (Delayed xf)

    val empty = Nil

    fun cons (x, xf) = Cons (x, delay xf);

    fun force xp = case !xp of
                     Delayed f => let val s = f ()
                                  in xp := s; s end
                   | s => s

    fun null Nil      = true
      | null (Cons _) = false

    fun hd Nil            = raise Empty
      | hd (Cons (x, _))  = x

    fun tl Nil            = raise Empty
      | tl (Cons (_, xf)) = force xf

    fun take (xq, 0)           = []
      | take (Nil, n)          = []
      | take (Cons (x, xp), n) = x :: take (force xp, n-1)

    fun toList Nil            = []
      | toList (Cons (x, xp)) = x::(toList (force xp))

    fun fromList []           = Nil
      | fromList (x::xs)      = Cons (x, delay (fn()=> fromList xs))

    fun            Nil @ yq = yq
      | (Cons (x, xp)) @ yq = Cons (x, delay (fn()=> (force xp) @ yq))

    fun interleave (Nil, yq) = yq
      | interleave (xq, Nil) = xq
      | interleave (Cons (x, xp), yq) =
            Cons (x, delay (fn ()=> interleave (yq, force xp)))

    fun concat Nil = Nil
      | concat (Cons (Nil, xp)) = concat (force xp)
      | concat (Cons (Cons (x, xp), yqq)) =
            Cons (x, delay (fn()=> concat (Cons (force xp, yqq))))

    fun map f Nil            = Nil
      | map f (Cons (x, xp)) = Cons (f x, delay (fn()=> map f (force xp)))

    fun filter p Nil = Nil
      | filter p (Cons (x, xp)) = if p x
            then Cons (x, delay (fn()=> filter p (force xp)))
            else filter p (force xp)

    fun cycle seqfn = let val knot = ref Nil
                      in knot := seqfn (fn()=> !knot); !knot end

    fun generate f = case f () of
                       NONE   => Nil
                     | SOME v => Cons (v, delay (fn ()=> generate f))
end;
