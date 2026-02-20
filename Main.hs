module A2.Main where
import Prelude hiding ((+), (-), (*), (/), negate, recip, (^),
                pi, sin, cos, exp, fromInteger, fromRational)

import Algebra
import FunExp

type Tri a = (a, a, a)
type TriFun a = Tri (a -> a) -- = (a -> a, a -> a, a -> a)
type FunTri a = a -> Tri a -- = a -> (a, a, a)

instance Additive a => Additive (Tri a) where
  (+) = addTri; zero = zeroTri

instance (Additive a, Multiplicative a) => Multiplicative (Tri a) where
  (*) = mulTri; one = oneTri

instance AddGroup a => AddGroup (Tri a) where
  negate = negateTri

instance (AddGroup a, MulGroup a) => MulGroup (Tri a) where
  recip = recipTri



{- 
  f+g         = f + g
  d/dx (f+g)  = f' + g'
  d/d2x (f+g) = f'' + g''
-}
addTri :: Additive a => Tri a -> Tri a -> Tri a
addTri (a0, a1, a2) (b0, b1, b2) = (a0+b0, a1+b1, a2+b2)


{-
  0       = 0
  d/dx 0  = 0
  d/d2x 0 = 0
-}
zeroTri :: Additive a => Tri a
zeroTri = (zero, zero, zero)

{-
  f*g = f*g
  d/dx (f*g) = f'*g + f*g'
  d/d2x (f*g) = f''*g + 2f'*g' + f*g'' => f''*g + f'*g' + f'*g' + f*g'' 
-}
mulTri :: (Additive a, Multiplicative a) => Tri a -> Tri a -> Tri a
mulTri (a0, a1, a2) (b0, b1, b2) = (a0*b0, (a1*b0)+(a0*b1), (a2*b0)+(a1*b1)+(a1*b1)+(a0*b2))

{-
  1 = 1
  d/dx 1 = 0
  d/d2x 1 = 0
-}
oneTri :: (Additive a, Multiplicative a) => Tri a
oneTri = (one, zero, zero)

{-
  -f = -f
  d/dx (-f) = -f'
  d/d2x (-f) = -f''
-}
negateTri :: AddGroup a => Tri a -> Tri a
negateTri (a0, a1, a2) = (negate a0, negate a1, negate a2)

{-
   1/f = 1/f
   d/dx (1/f) = (-f'/(f*f))
   d/d2x (1/f) = (2*f'*f'-f*f'')/(f*f*f)
-}
recipTri :: (AddGroup a, MulGroup a) => Tri a -> Tri a
recipTri (a0, a1, a2) = (recip a0, negate a1 / (a0*a0), ((a1*a1+a1*a1) - (a0*a2)) / (a0*a0*a0) )

instance Transcendental a => Transcendental (Tri a) where
  pi = piTri; sin = sinTri; cos = cosTri; exp = expTri

{-
  pi = pi
  d/dx pi = 0
  d/d2x pi = 0
-}
piTri :: Transcendental a => Tri a
piTri = (pi, zero, zero)

{-
  sin (f) = sin (f)
  d/dx sin (f) = f' * cos (f)
  d/d2x sin = f''*cos(f)-sin(f)*f'*f'
-}
sinTri :: Transcendental a => Tri a -> Tri a
sinTri (a0, a1, a2) = (sin a0, a1 * cos a0, (a2 * cos a0) - (a1 * a1 * sin a0))

{-
  cos (f) = cos (f)
  d/dx cos (f) = -f' * sin (f)
  d/d2x cos (f) = -f'' * sin (f) - cos (f) * f' * f'
-}
cosTri :: Transcendental a => Tri a -> Tri a
cosTri (a0, a1, a2) = (cos a0, negate a1 * sin a0, (negate a2 * sin a0) - (a1 * a1 * cos a0))

{-
  e^f = e^f
  d/dx e^f = f'*e^f
  d/d2x e^f = f'' * e^f + f' * f' * e^f = e^f * (f'*f' + f'')
-}
expTri :: Transcendental a => Tri a -> Tri a
expTri (a0, a1, a2) = (exp a0, a1 * exp a0, (a1*a1 + a2) * exp a0)

--Testing implementations
xTri :: Transcendental a => a -> Tri a
xTri x = (x, one, zero)

cTri :: Transcendental a => a -> Tri a
cTri c = (c,zero,zero)


--Should return (1,0,0) for every x we pass in
testTrigIdentity :: Transcendental a => Tri a -> Tri a 
testTrigIdentity x = sin x * sin x + cos x * cos x
