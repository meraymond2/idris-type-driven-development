module Shape_abs

export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

export
triangle : Double -> Double -> Shape
triangle = Triangle

export
rectangle : Double -> Double -> Shape
rectangle = Rectangle

export
circle : Double -> Shape
circle = Circle

public export
data ShapeView : Shape -> Type where
  STriangle : (base : Double) -> (height : Double) -> ShapeView (triangle base height)
  SRectangle : (base : Double) -> (height : Double) -> ShapeView (rectangle base height)
  SCircle : (radius : Double) -> ShapeView (circle radius)

-- I don't think this serves any purpose besides practice,
-- you could get the same result if you did public export
-- Shape because the view is the same as the constructor
export
shapeView : (shape : Shape) -> ShapeView shape
shapeView (Triangle base height) = STriangle base height
shapeView (Rectangle base height) = SRectangle base height
shapeView (Circle radius) = SCircle radius
