{-# Language QuasiQuotes #-}

module Examples where


import Verbatim 
import VerbatimParser


liftS :: String -> Verbatim
liftS = PlainDat 
 
main = do
--      print "hello"
      writeFile "LOL" $ printVerbatim $ script1 (liftS "FunArg1")  (liftS "32")
--      putStrLn $ printVerbatim $ script2 . liftS . show $ [0..4] 

script1 m n = [verbatim|
#! /usr/bin/Funverbatim
Hi there Alessio I'm a funny verbatim script
for computing funny stuff with some 
funny argument like %m and %n
|]
  
script2 xs = [verbatim|random_Path 
what if I want to print a list ?
well for the moment lets just show the list like %xs|]


-- QuasiQuoter quotePat is  [verbatim| $n |] (pattern) in this expression
-- script3 [verbatim| $n |] = [verbatim| some scripts|]

-- QuasiQuoter quoteDec would be this one:
-- [verbatim| NewFun = Exp1 Exp2 |]

-- QuasiQuoter quoteType would be here:



