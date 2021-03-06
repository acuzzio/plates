{-# Language QuasiQuotes #-}

module Main where

import Text.Printf
import System.Environment
import Verbatim
import VerbatimParser

main = do
  args <- getArgs
  case length args of
       6 -> do
            let  [plateT', x1', x2', dens', dim', totCharge'] = args
                 plateT    = read plateT' :: PlateType
                 x1        = read2 x1'
                 x2        = read2 x2'
                 dens      = floor $ read2 dens'
                 dim       = read2 dim'
                 totCharge = read2 totCharge'
            piastrami plateT x1 x2 dens dim totCharge
       otherwise -> putStrLn $ printVerbatim errMessage 

errMessage = [verbatim|

Plates program usage:
$ Plates plateType   dim1  dim2 howManyPointsInDiameter  plateRadius plateTotalCharge
$ Plates     X       -5.0  6.0           100                15.0          1.0

For example
$ Plates X -5.0 6.0 100 15.0 1.0

Available plateTypes:
X - 2 circular plates in X axis
Z - 2 circular plates in Z axis

|]

data PlateType = X | Z deriving (Show, Read)

piastrami :: PlateType -> Double -> Double -> Int -> Double -> Double -> IO()
piastrami pt x x2 dens dim totalCharge = do
  let charges      = generatePoints pt x x2 dens dim totalCharge 
--      strings      = map (map show) charges
--      reformatZ    = map (replace 3) strings 
      reformatZ    = map (map printW) charges
      toFile       = unlines $ map unwords $ reformatZ
      howMany      = show $ length charges
      toSee        = unlines $ map unwords $ map (\x -> ["H",x!!0,x!!1,x!!2]) reformatZ
      message1     = electricField (abs (x2 - x)) dim
      message2     = dypoleMoment totalCharge x x2
  putStrLn $ message1 ++ message2
  writeFile "piastra" $ howMany ++ "\n" 
  appendFile "piastra" $ toFile
  writeFile "toSee.xyz" $ howMany ++ "\n\n"
  appendFile "toSee.xyz" $ toSee

generatePoints :: PlateType -> Double -> Double -> Int -> Double -> Double -> [[Double]]
generatePoints pt x x2 dens dimD totalCharge = case pt of
  X -> let singleCharge = totalCharge / (fromIntegral pointsN :: Double)
           points       = [[x, y, z, singleCharge, 0.0, 0.0, 0.0] | y <- listar (-dimD) dimD dens, z <- listar (-dimD) dimD dens, lengthInferior y z dimD]
           points2      = [[x2, y, z, (-singleCharge), 0.0, 0.0, 0.0] | y <- listar (-dimD) dimD dens, z <- listar (-dimD) dimD dens, lengthInferior y z dimD]
           pointsN      = length points
       in points ++ points2  
  Z -> let singleCharge = totalCharge / (fromIntegral pointsN :: Double)
           points       = [[z, y, x, singleCharge, 0.0, 0.0, 0.0] | y <- listar (-dimD) dimD dens, z <- listar (-dimD) dimD dens, lengthInferior y z dimD]            
           points2      = [[z, y, x2, (-singleCharge), 0.0, 0.0, 0.0] | y <- listar (-dimD) dimD dens, z <- listar (-dimD) dimD dens, lengthInferior y z dimD]
           pointsN      = length points
       in points ++ points2 


reformat :: String -> String
reformat x = let float = read x :: Double
             in printf  "%3.8f" float :: String

printW x = printf  "%3.8f" x :: String

replace :: Int -> [String] -> [String]
replace position list = let 
  first   = take (position) list
  second  = drop (position+1) list
  element = reformat (list !! position)
  in first ++ [element] ++ second

lengthInferior :: Double -> Double -> Double -> Bool
lengthInferior y z ray = let length = sqrt (y**2 + z**2)
                         in if length > ray then False else True

electricField :: Double -> Double -> String 
electricField x dimension = let 
      mToAng       = 1.0e-10
      elecCharge   = 1.6e-19
      epsilZero    = 8.85e-12
      surface      = pi * (dimension **2.0)
      surfaceM2    = surface / mToAng
      charge       = elecCharge * x
      field        = (charge / surfaceM2)/epsilZero
      r            = sqrt $ 1 / (4 * pi * (x/surface))
      in "\n" ++ (show field) ++ " Newton/Coulomb or volt/Meteri\nElectric field equivalent to a charge of an Ion/Electron at " ++ (show r) ++ " Angstrom\n" 

dypoleMoment :: Double -> Double -> Double -> String
dypoleMoment charge x x2 = let
      distance = abs ( x - x2 )
      mom      = distance * charge
      in (show mom) ++ " Debye (electron charge * Angstrom)"

listar :: Double -> Double -> Int -> [Double]
listar max min res = let
                    deno = fromIntegral res
                    step  = (max-min)/(deno-1.0)
                    in take (res) $ iterate (+step) min

read2 x = read x :: Double
read3 x = read x :: Int
