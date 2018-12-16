-- Let in Idris works like Clojure

-- (let [x 77
--       y 11]
--   x + 7) => 88

useLet : Int
useLet =
  let x = 77
      y = 11 in
      x + y -- 88
