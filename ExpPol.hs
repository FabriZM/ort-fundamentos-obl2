{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs#-}

module ExpPol where

import Polinomios

-- ========================
-- EXPRESIONES POLINOMICAS
-- ========================

data ExPol where 
		Pol  :: Polinomio -> ExPol 
		Der  :: ExPol -> ExPol 
		Eval :: ExPol -> Int -> ExPol 
		Sum  :: [ExPol] -> ExPol 
		Prod :: [ExPol] -> ExPol 
				deriving Show

																	
--10) 
cantPol :: ExPol -> Int
cantPol = \t -> case t of {
	Pol p -> 1;
	Der d -> cantPol d;
	Eval a n -> 1;
	Sum y -> sum (map cantPol y);
	Prod x -> sum (map cantPol x); 
}

--11)
cantx :: ExPol -> Int
cantx  = \t -> case t of {
	Pol p -> length p;
	Der d -> cantx d;
	Eval a n -> 1;
	Sum y -> sum (map cantx y);
	Prod x -> sum (map cantx x); 
}

--12)
maxProd :: ExPol -> Int
maxProd  = undefined


--13)
gradoEP :: ExPol -> Int
gradoEP = undefined 
	
--14)	
calcEP :: ExPol -> Polinomio
calcEP = undefined  

--15)
resultado :: ExPol -> String
resultado = undefined

