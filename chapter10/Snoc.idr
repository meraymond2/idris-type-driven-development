data SnocList ty = Empty | Snoc (SnocList ty) ty

reverseSnoc : SnocList ty -> List ty
reverseSnoc Empty = []
reverseSnoc (Snoc list x) = x :: reverseSnoc list
