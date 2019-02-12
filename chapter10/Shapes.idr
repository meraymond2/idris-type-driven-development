import ShapeMod

area : Shape -> Double
area shape with (shapeView shape)
  area (triangle base height) | (STriangle base height) = 0.5 * base * height
  area (rectangle base height) | (SRectangle base height) = base * height
  area (circle radius) | (SCircle radius) = pi * radius * radius
