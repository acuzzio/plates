{-# Language QuasiQuotes #-}

module Main where

import Text.Printf
import System.Environment
import Verbatim
import VerbatimParser

main = do
  args <- getArgs
  case length args of
       5 -> do
            let  [plateT', x1', x2', dim', totCharge'] = args
                 plateT    = read plateT' :: PlateType
                 x1        = read2 x1'
                 x2        = read2 x2'
                 dim       = floor $ read2 dim'
                 totCharge = read2 totCharge'
            piastrami plateT x1 x2 dim totCharge
       otherwise -> putStrLn $ printVerbatim errMessage 

errMessage = [verbatim|
Please write:
$ Plates plateType x1 x2 radius charge
$ Plates X -5.0 6.0 15.0 1.0

avaiable plateTypes:
X - 2 circular plates in X axis
Z - 2 circular plates in Z axis

|]

data PlateType = X | Z deriving (Show, Read)

piastrami :: PlateType -> Double -> Double -> Integer -> Double -> IO()
piastrami pt x x2 dim totalCharge = do
  let charges      = generatePoints pt x x2 dim totalCharge 
      strings      = map (map show) charges
      reformatZ    = map (replace 3) strings 
      toFile       = unlines $ map unwords $ reformatZ
      howMany      = show $ length charges
      toSee        = unlines $ map unwords $ map (\x -> ["H",x!!0,x!!1,x!!2]) strings
  electricField (abs (x2 - x)) dim
  dypoleMoment totalCharge x x2
  writeFile "piastra" $ howMany ++ "\n" 
  appendFile "piastra" $ toFile
  writeFile "toSee.xyz" $ howMany ++ "\n\n"
  appendFile "toSee.xyz" $ toSee

generatePoints pt x x2 dim totalCharge = case pt of
  X -> let singleCharge = totalCharge / (fromIntegral pointsN :: Double)
           points       = [[x, y, z, singleCharge, 0.0, 0.0, 0.0] | y <- map (\x -> fromInteger x :: Double) [(-dim)..dim], z <- map (\x -> fromInteger x :: Double) [(-dim)..dim], lengthInferior y z (fromInteger dim :: Double)]
           points2      = [[x2, y, z, (-singleCharge), 0.0, 0.0, 0.0] | y <- map (\x -> fromInteger x :: Double) [(-dim)..dim], z <- map (\x -> fromInteger x :: Double) [(-dim)..dim], lengthInferior y z (fromInteger dim :: Double)]
           pointsN      = length points
       in points ++ points2  
  Z -> let singleCharge = totalCharge / (fromIntegral pointsN :: Double)
           points       = [[z, y, x, singleCharge, 0.0, 0.0, 0.0] | y <- map (\x -> fromInteger x :: Double) [(-dim)..dim], z <- map (\x -> fromInteger x :: Double) [(-dim)..dim], lengthInferior y z (fromInteger dim :: Double)]            
           points2      = [[z, y, x2, (-singleCharge), 0.0, 0.0, 0.0] | y <- map (\x -> fromInteger x :: Double) [(-dim)..dim], z <- map (\x -> fromInteger x :: Double) [(-dim)..dim], lengthInferior y z (fromInteger dim :: Double)]
           pointsN      = length points
       in points ++ points2 


reformat :: String -> String
reformat x = let float = read x :: Double
             in printf  "%3.4f" float :: String

replace :: Int -> [String] -> [String]
replace position list = let 
  first   = take (position) list
  second  = drop (position+1) list
  element = reformat (list !! position)
  in first ++ [element] ++ second

lengthInferior :: Double -> Double -> Double -> Bool
lengthInferior y z ray = let length = sqrt (y**2 + z**2)
                         in if length > ray then False else True

electricField :: Double -> Integer -> IO()
electricField x dimension = do
  let mToAng       = 1.0e-10
      elecCharge   = 1.6e-19
      epsilZero    = 8.85e-12
      surface      = pi * ((fromIntegral dimension)**2.0)
      surfaceM2    = surface / mToAng
      charge       = elecCharge * x
      field        = (charge / surfaceM2)/epsilZero
      r            = sqrt $ 1 / (4 * pi * (x/surface))
  putStrLn $ "\n" ++ (show field) ++ " Newton/Coulomb or volt/Meteri\n"
  putStrLn $ "Electric field equivalent to a charge of an Ion/Electron at " ++ (show r) ++ " Angstrom\n" 

dypoleMoment :: Double -> Double -> Double -> IO()
dypoleMoment charge x x2 = do
  let distance = abs ( x - x2 )
      mom      = distance * charge
  putStrLn $ "\n" ++ (show mom) ++ " Debye (electron charge * Angstrom)\n"

read2 x = read x :: Double
read3 x = read x :: Integer
