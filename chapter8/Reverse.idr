import Data.Vect

-- the rewrite tells the compiler that plus k 1 = plus (S k)
-- so you can use the former where it expects the latter
my_reverse : Vect n elem -> Vect n elem
my_reverse [] = []
my_reverse {n = S k} (x :: xs) = let reversed = my_reverse xs ++ [x]
                                  in rewrite plusCommutative 1 k
                                  in reversed

-- Note that when case-splitting on this, it'll tell you
-- that it's impossible.
reverseProof : Vect (k + 1) elem -> Vect (S k) elem
reverseProof {k} xs = rewrite plusCommutative 1 k in xs

my_reverse2 : Vect n elem -> Vect n elem
my_reverse2 [] = []
my_reverse2 {n = S k} (x :: xs) = reverseProof (my_reverse xs ++ [x])

-- This works, because it's looking for Vect (0 + m) elem,
-- and plus knows what to do with that.
my_append : Vect n elem -> Vect m elem -> Vect (n + m) elem
my_append [] ys = ys
my_append (x :: xs) ys = x :: my_append xs ys

-- This doesn't work the same, cause you end up with
-- (plus m 0), and it doesn't know how to match on m,
-- unless you tell it to
my_append2 : Vect n elem -> Vect m elem -> Vect (m + n) elem
my_append2 [] ys = rewritePlusZero ys where
                   rewritePlusZero : Vect m elem -> Vect (plus m 0) elem
                   rewritePlusZero {m} xs = rewrite plusZeroRightNeutral m in xs
my_append2 (x :: xs) ys = rewriteRightSucc (x :: my_append2 xs ys) where
                          rewriteRightSucc : Vect (S (plus m len)) elem -> Vect (plus m (S len)) elem
                          rewriteRightSucc {m} {len} xs = rewrite sym (plusSuccRightSucc m len) in xs
                          rewriteRightSucc {m} {len} xs = rewrite sym (plusSuccRightSucc m len) in xs
{- pSRS has S(l + r) = l + (S r)
 sym reverses the rewrite rule (left = right) -> right = left
 so its l + (S r) = S(l + r)
 I think what you want to to change the return type to match the type on the left,
 rather than change the left to match the return, I think
 without sym you get the error
 `rewriting S (plus m len) to plus m (S len)
  did not change type Vect (plus m (S len)) elem`
-}

{-
1. Starting with something you want to prove
  myPlusCommutes Z m = ?x where x : m = plus m 0
2. You need to bring a rewrite rule into scope, that proves the reverse.
  rewrite (plusZeroRightNeutral m) in ?x
    where _rewrite_rule : plus m 0 = m
          x : m = m
3. And since m already equals m, you can just do in Refl.
  rewrite (plusZeroRightNeutral m) in Refl

  I still not clear on this. I don't know why the expected type
  is S (plus k m) = plus m (S k)
-}
total
myPlusCommutes : (n : Nat) -> (m : Nat) -> plus n m = plus m n
myPlusCommutes Z m = rewrite (plusZeroRightNeutral m) in Refl
myPlusCommutes (S k) m = rewrite plusSuccRightSucc k m in
                         rewrite myPlusCommutes k (S m) in
                         rewrite plusSuccRightSucc m k in Refl

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
        reverse' {n} acc [] = rewrite plusZeroRightNeutral n in acc
        reverse' {n} {m = (S len)} acc (x :: xs) =
          rewrite sym (plusSuccRightSucc n len) in (reverse' (x :: acc) xs)
