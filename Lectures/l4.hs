module Lecture4 where


data MyNatural = Zero | Succ MyNatural
  deriving (Eq, Show)
--  deriving (Eq)

two   = Succ $ Succ Zero
three = Succ two


natPlus Zero  y       = y
natPlus (Succ x)  y   = Succ (natPlus x y)

natMinus x Zero   = x
natMinus Zero y   = error "Negative Natural"
natMinus (Succ x) (Succ y) = natMinus x y


natTimes  Zero y    = Zero
natTimes (Succ x) y = natPlus y (natTimes x y)

natSignum  Zero    = Zero
natSignum (Succ x) = Succ Zero

integerToNat 0     = Zero
integerToNat x     = Succ (integerToNat (x-1))


instance Num MyNatural where
  (+) = natPlus
  (-) = natMinus
  (*) = natTimes
  negate = error "Negative natural"
  abs x = x
  signum = natSignum
  fromInteger = integerToNat


------------------------------------------

showNat n = show (intValue n)
  where
  intValue  Zero    = 0
  intValue (Succ x) = 1 + intValue x


-- instance Show MyNatural where show = showNat

-----------------------------------------

type ListNatural = [()]

twoL   = [(),()]
threeL = [(),(),()]

f1 x y = foldr (:) x y
f2 x y = foldr (const (f1 x)) [] y
f3 x y = foldr (const (f2 x)) [()] y


-----------------------------------------


type ChurchNatural a = (a -> a) -> (a -> a)

zeroC, oneC, twoC :: ChurchNatural a
zeroC f = id         -- zeroC = const id
oneC  f = f          -- oneC  = id
twoC  f = f.f


succC n f =  f.(n f)
threeC = succC twoC


plusC x y f   = (x f).(y f)
timesC x y    = x.y
expC x y      = y x


showC x = show $ (x (+1)) 0


pc = showC $ plusC twoC threeC
tc = showC $ timesC twoC threeC
xc = showC $ expC twoC threeC

