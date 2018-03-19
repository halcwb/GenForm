namespace Informedica.GenUtils.Lib

/// Helper functions for `BigRational`
module BigRational = 

    open MathNet.Numerics

    /// Message type
    type Message =
        | CannotMatchOperator
        | CannotDivideByZero
        | CannotParseString of string
    
    /// Exception type
    exception BigRationalException of Message

    /// Raise exception with message `m`
    let raiseExc m = m |> BigRationalException |> raise

    /// Apply a `f` to bigrational `x`
    let apply f (x: BigRational) = f x

    /// Utility to enable type inference
    let get = apply id

    /// Parse a string to a bigrational
    let parse s = 
        try 
            s |> BigRational.Parse
        with
        | _ -> s |> CannotParseString |> raiseExc


    /// Parse a string and pass the result 
    /// either to `succ` or `fail`
    let parseCont succ fail s =
        try 
            parse s |> succ
        with
        | BigRationalException(m) -> m |> fail

    /// Try to parse a string and 
    /// return `None` if it fails 
    /// otherwise `Some` bigrational
    let tryParse s = 
        try 
            s |> parse |> Some 
        with 
        | _ -> None

    /// Create a bigrational from an int
    let fromInt = BigRational.FromInt

    /// Get the greatest common divisor
    /// of two bigrationals `a` and `b`
    let gcd (a : BigRational) (b: BigRational) =
        let den = a.Denominator * b.Denominator
        let rec gcd' a' b' =
            match b' with
            | _  when b' = 0I -> abs a'
            | _ -> gcd' b' (a' % b')
        let num = gcd' (a.Numerator * b.Denominator) (b.Numerator * a.Denominator)
        (num |> BigRational.FromBigInt) / (den |> BigRational.FromBigInt)

    /// Convert a bigrational to a string
    let toString v = (v |> get).ToString()

    /// Convert an optional `BigRational` to a `string`.
    /// If `None` then return empty `string`.
    let optToString = function
        | Some v' -> v' |> toString
        | None    -> ""

    /// Convert `n` to a multiple of `d`.
    /// Passes an `CannotDivideByZero` message
    /// to `fail` when `d` is zero.
    let toMultipleOfCont succ fail d n  =
        if d = 0N then CannotDivideByZero |> fail
        else
            let m = (n / d) |> BigRational.ToInt32 |> BigRational.FromInt
            if m * d < n then (m + 1N) * d else m * d
            |> succ
    
    /// Convert `n` to a multiple of `d`.
    /// Raises an `CannotDivideByZero` message
    /// exception when `d` is zero.
    let toMultipleOf n d = toMultipleOfCont id raiseExc n d

    /// Checks whether `v` is a multiple of `incr`
    let isMultiple incr v =
        let incr, v = incr |> get, v |> get 
        (v.Numerator * incr.Denominator) % (incr.Numerator * v.Denominator) = 0I

    /// Constant 0
    let zero = 0N

    /// Constant 1
    let one = 1N

    /// Constant 2
    let two = 2N

    /// Constant 3
    let three = 3N

    /// Check whether the operator is subtraction
    let opIsSubtr op = (three |> op <| two) = three - two // = 1

    /// Check whether the operator is addition
    let opIsAdd op   = (three |> op <| two) = three + two // = 5

    /// Check whether the operator is multiplication
    let opIsMult op  = (three |> op <| two) = three * two // = 6

    /// Check whether the operator is divsion
    let opIsDiv op   = (three |> op <| two) = three / two // = 3/2

    /// Match an operator `op` to either
    /// multiplication, division, addition
    /// or subtraction. </br> 
    /// Raises a `CannotMatchOperator` message 
    /// exception when no match.
    let (|Mult|Div|Add|Subtr|) op =
        match op with
        | _ when op |> opIsMult  -> Mult
        | _ when op |> opIsDiv   -> Div
        | _ when op |> opIsAdd   -> Add
        | _ when op |> opIsSubtr -> Subtr
        | _ -> CannotMatchOperator |> raiseExc



