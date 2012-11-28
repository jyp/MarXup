{-# OPTIONS_GHC -pgmF marchup -F #-}

import MarchUp.SimpleText

val :: Int
val = 3

sieve (p:xs) = p : sieve [x |x <- xs, x `mod` p /= 0]
primes = sieve [2..]

text = @"
This is an example of simple text with integer elements.
For example, val = @val.
Also, these are primes: @take(10)(primes)
These are also primes:  @{take 10 $ drop 1000 $ primes}
@"

main = print text


