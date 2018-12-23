data PowerSource = Petrol | Pedal

-- sometimes called a family of types
-- since you're defining multiple related types
-- (it seems a little like inheritance, except that
-- it keeps behaviour completely separate)
data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol

-- has a type variable power, cause
-- this works for all vehicles
wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

-- restricts the type
refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel Bicycle impossible
