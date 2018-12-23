data PowerSource = Petrol | Pedal | Elec

-- sometimes called a family of types
-- since you're defining multiple related types
-- (it seems a little like inheritance, except that
-- it keeps behaviour completely separate)
data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol
     Unicycle : Vehicle Pedal
     Motorcycle : (fuel : Nat) -> Vehicle Petrol
     Tram : (power : Nat) -> Vehicle Elec

-- has a type variable power, cause
-- this works for all vehicles
wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2
wheels Tram = 0

-- restricts the type
refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 75
refuel Bicycle impossible -- not necessary
refuel Unicycle impossible -- just documentation
