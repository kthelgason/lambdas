
to_number n = n (\x -> x + 1) 0
to_bool   p = p True False

increment = \n -> \f -> \x -> f (n f x)

zero   = \f -> \x -> x
one    = \f -> \x -> f x
two    = increment one
three  = increment two

true   = \t -> \f -> t
false  = \t -> \f -> f

iif = id

pair  = \x -> \y -> \f -> f x y
left  = \p -> p (\x -> \y -> x)
right = \p -> p (\x -> \y -> y)

slide = \p -> pair (right p) (increment $ right p)
decrement = \n -> left $ n slide (pair zero zero)

add      = \a -> \b -> a increment b
subtract = \a -> \b -> a decrement b
multiply = \a -> \b -> b (add a) zero

is_zero  = \n -> n (\x -> false) true


