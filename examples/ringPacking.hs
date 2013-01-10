{-# LANGUAGE NoMonomorphismRestriction #-}

-- <http://mathlesstraveled.com/2012/10/05/factorization-diagrams/>
-- <http://mathlesstraveled.com/2012/11/05/more-factorization-diagrams/>


import Data.List

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Math.NumberTheory.Primes.Factorisation (factorise)
import Math.NumberTheory.Primes.Sieve (primes)

radiusFromCount :: Int -> Double
radiusFromCount 0 = 0
radiusFromCount 1 = 0
radiusFromCount m = 1 / sin (pi / fromIntegral m)

packRing m b d = ring <> boundary
  where ring = mconcat $ map f [0..m-1]
        boundary = b # scale ((r+1)*s)
        f i = d # translateX (r*s) # rotateBy (fromIntegral i / fromIntegral m)
        r = radiusFromCount m
        s = magnitude (envelopeV unit_X d) -- envelopeS ?

fitsIn inner outer = radiusFromCount outer - radiusFromCount inner >= 2
---- equivalent definition
--fitsIn 0 _ = True
--fitsIn 1 m' = m' >= 6
--fitsIn 2 m' = m' >= 10
--fitsIn m m' = m' - m >= 7

ringCount :: Int -> Int
ringCount = (map ringCount' [0..] !!)
ringCount' 0 = 0
ringCount' 1 = 1
ringCount' n = outer + increment
  where increment = if inner `fitsIn` outer then 0 else 1
        outer = ringCount (n - 1)
        inner = ringCount (n - outer)

ringPartition 0 = []
ringPartition n = m : ringPartition (n-m)
  where m = ringCount n

packRings n b d = mconcat $ reverse [ packRing m b d | m <- ringPartition n ]

nest pack [] _ d = d
nest pack (n:ns) (b:bs) d = pack n b (nest pack ns bs d)

--factors = concatMap (uncurry $ flip replicate) . factorise
factors n = concat [ replicate a f | (f, a) <- factorise n ]
--factors = map (uncurry (^)) . factorise
factors' n = [ f^a | (f, a) <- factorise n ]
-- number of prime factors
npf = sum . map snd . factorise
-- number of distinct prime factors
ndpf = length . factorise

factorDiagram pack n bs d = nest pack (reverse $ map fromIntegral $ factors n) bs d
powerFactorDiagram pack n bs d = nest pack (reverse $ map fromIntegral $ factors' n) bs d

bagSelect [] = []
bagSelect ((x, 1):b) = (x, b):[ (y, (x, 1):b') | (y, b') <- bagSelect b ]
bagSelect ((x, n):b) = (x, (x, n-1):b):[ (y, (x, n):b') | (y, b') <- bagSelect b ]

bagPermutations [] = [[]]
bagPermutations b = [ x:ys | (x, b') <- bagSelect b, ys <- bagPermutations b' ]

dot = circle 1 # lw 0 # fc black
gdot = circle 1 # lw 0 # fc grey
gdots = cycle [ circle 1 # lw 0 # fc c | c <- [ grey, white ] ]
rainbow = [ red, orange, yellow, green, blue, indigo, violet ]
coldots = cycle [ circle 1 # lw 0 # fc c | c <- rainbow ]
dots = coldots

--main = defaultMain (packRings 20 dots dot)
--main = defaultMain (packRings 7 (packRings 3 gdot dot))
--main = defaultMain (nest packRings [7, 5, 3] gdot dot)
numlabel n = text (show n) <> circle 1
numbers ns = cat unit_Y [ numlabel n === packRings n (dots!!(npf (fromIntegral n)+1)) dot | n <- ns ]
prim maxp = cat unit_Y [ numlabel n === packRings (fromIntegral n) gdot dot
                       | n <- takeWhile (<maxp) primes ]
fd n = factorDiagram packRings n dots dot
pfd n = powerFactorDiagram packRings n dots dot
factorisations ns = cat unit_Y [ numlabel n === fd n | n <- ns ]
powerfactorisations ns = cat unit_Y [ numlabel n === pfd n | n <- ns ]
table = vcat [ hcat [ fd (10*i+j+1) # scaleUToY 0.8 <> square 1 | j <- [0..9] ] | i <- [0..19] ]
powertable = vcat [ hcat [ pfd (10*i+j+1) # scaleUToY 0.8 <> square 1 | j <- [0..9] ] | i <- [0..19] ]
allfactorisations ns = cat unit_Y [ numlabel n
                                    ===
                                    cat unitX [ nest packRings (map fromIntegral p) dots dot
                                              | p <- bagPermutations $ factorise n ]
                                  | n <- ns ]
allpowerfactorisations ns = cat unit_Y [ numlabel n
                                         ===
                                         cat unitX [ nest packRings (map fromIntegral p) dots dot
                                                   | p <- permutations $ factors' n ]
                                       | n <- ns ]
years = (((text "2012" # fc white # fontSize 10) <> (dot # scale (sqrt 2012))) |||
         (numbers [2012] # centerY) |||
         (allfactorisations [2012]) # centerY) ===
        (((text "2013" # fc white # fontSize 10
          ) <> (dot # scale (sqrt 2013))) |||
         (numbers [2013] # centerY) |||
         (allfactorisations [2013]) # centerY)
                            
--main = defaultMain allfactorisations
main = multiMain [ ("numbers", numbers [1..60]),
                   ("primes", prim 60),
                   ("years", years),
                   ("factorisations", factorisations [1..60]),
                   ("powerfactorisations", powerfactorisations [1..60]),
                   ("table", table),
                   ("powertable", powertable),
                   ("allfactorisations", allfactorisations [1..60]),
                   ("allpowerfactorisations", allpowerfactorisations [1..60]) ]
