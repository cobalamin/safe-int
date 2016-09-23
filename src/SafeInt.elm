module SafeInt exposing (SafeInt, fromInt, get, map, map2, andThen, invalid, (+!), (-!), (*!), (//!), (%!), (^!))

{-| This module is a way of explicitly representing invalid integers so that you can ensure they don't propagate to weird places in your application (for instance the view). It achieves this by internally modelling which numbers are valid integers and which aren't, and returning a `Maybe Int` from the `get` function. So in effect, you always need to explicitly handle invalid calculations at the end of a calculation chain.

# Creating and retrieving
@docs SafeInt, fromInt, get

# Calculations
@docs (+!), (-!), (*!), (//!), (%!), (^!)

# Wrapping your own functions
@docs map, map2, andThen, invalid
-}

{-| A type representing an explicitly valid or invalid `Int`
-}
type SafeInt
    = Safe Int
    | Invalid


{-| A value representing an invalid `Int`.

This is largely only useful when writing your own calculations with `andThen` and you want to be able to say "this would be an invalid `Int`" yourself. See the `andThen` docs for an example.
-}
invalid : SafeInt
invalid =
    Invalid


{-| Creates a new `SafeInt` based on an `Int` value.
-}
fromInt : Int -> SafeInt
fromInt i =
    if intIsNaN i || intIsInfinite i then
        Invalid
    else
        Safe i


{-| Try to retrieve a valid `Int` from a `SafeInt`. Returns a `Just` value if the wrapped integer is valid, `Nothing` if it is invalid.
-}
get : SafeInt -> Maybe Int
get si =
    case si of
        Invalid ->
            Nothing

        Safe i ->
            Just i


infinity : Int
infinity =
    floor (1 / 0)


negInfinity : Int
negInfinity =
    floor (-1 / 0)


intIsNaN : Int -> Bool
intIsNaN x =
    x /= x


intIsInfinite : Int -> Bool
intIsInfinite x =
    x == infinity || x == negInfinity


{-| Use a function operating on one `Int` value with one `SafeInt`.
-}
map : (Int -> Int) -> SafeInt -> SafeInt
map f si =
    case si of
        Invalid ->
            Invalid

        Safe i ->
            fromInt (f i)

{-| Use a function operating on two `Int` values with two `SafeInt`s.

Most safe operators in this library (`(+!)`, `(-!)`, etc.) are implemented in terms of `map2`, which can give you an idea of what it does:

    (+!) = map2 (+)
-}
map2 : (Int -> Int -> Int) -> SafeInt -> SafeInt -> SafeInt
map2 op x y =
    case ( x, y ) of
        ( Safe x', Safe y' ) ->
            fromInt (op x' y')

        _ ->
            Invalid

{-| Put a `SafeInt` through a function that takes a valid `Int` value and returns a new `SafeInt`.

This is useful when you want to make advanced decisions about a calculation that is not in this library, and might lead to invalid results. It's especially useful when you chain two `andThen`s together to check one of two values in a binary operation, e.g. dividing by zero can be made safe like this.

    cantBe2Here : Int -> SafeInt
    cantBe2Here x =
        if x == 2 then
            invalid
        else
            fromInt x

    aSafeInt : SafeInt
    aSafeInt = fromInt 42

    result = someSafeInt `andThen` cantBe2Here
-}
andThen : SafeInt -> (Int -> SafeInt) -> SafeInt
andThen si f =
    case si of
        Invalid ->
            Invalid

        Safe i ->
            f i


andThen2 : (Int -> Int -> SafeInt) -> SafeInt -> SafeInt -> SafeInt
andThen2 f x y =
    x
        `andThen`
            \x' ->
                y
                    `andThen`
                        \y' ->
                            f x' y'


{-| A safe version of `(+)`
-}
(+!) : SafeInt -> SafeInt -> SafeInt
(+!) =
    map2 (+)

{-| A safe version of `(-)`
-}
(-!) : SafeInt -> SafeInt -> SafeInt
(-!) =
    map2 (-)

{-| A safe version of `(*)`
-}
(*!) : SafeInt -> SafeInt -> SafeInt
(*!) =
    map2 (*)



{-| A safe version of `(//)`

See the following GitHub issues as to what this treats as invalid:

- https://github.com/elm-lang/core/issues/590
- https://github.com/elm-lang/core/issues/92
-}
(//!) : SafeInt -> SafeInt -> SafeInt
(//!) =
    andThen2
        (\x y ->
            if y == 0 then
                Invalid
            else
                (toFloat x / toFloat y)
                    |> floor
                    |> fromInt
        )





{-| A safe version of `(^)`

See the following GitHub issues as to what this treats as invalid:

- https://github.com/elm-lang/core/issues/194
-}
(^!) : SafeInt -> SafeInt -> SafeInt
(^!) =
    andThen2
        (\x y ->
            if y < 0 then
                Invalid
            else
                fromInt (x ^ y)
        )

{-| A safe version of `(%)`
-}
(%!) : SafeInt -> SafeInt -> SafeInt
(%!) =
    map2 (%)
