

twiceTwoNotFive : 2 + 2 = 5 -> Void
twiceTwoNotFive Refl impossible

valNotSucc : (x : Nat) -> x = S x -> Void
valNotSucc _ Refl impossible
