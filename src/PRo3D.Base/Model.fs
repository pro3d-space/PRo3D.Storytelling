namespace PRo3D.Base

type Planet = 
  | Earth = 0
  | Mars  = 1
  | None  = 2

type SphericalCoo = {
  longitude : double
  latitude  : double
  altitude  : double
  radian    : double
}

