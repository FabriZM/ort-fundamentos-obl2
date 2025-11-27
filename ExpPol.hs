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
	Sum x -> sum (map cantPol x);
	Prod x -> sum (map cantPol x); 
}

--11)
cantx :: ExPol -> Int
cantx  = \t -> case t of {
	Pol p -> length (filter (> 0) (map snd p));
	Der d -> cantx d;
	Eval a n -> cantx a;
	Sum x -> sum (map cantx x);
	Prod x -> sum (map cantx x); 
}

--12)
maxProd :: ExPol -> Int
maxProd  = \t -> case t of {
	Pol p -> 0;
	Der d -> maxProd d;
	Eval a n -> 0;
	Sum x -> maximum (map maxProd x);
	Prod x -> maximum [length x, maximum (map maxProd x)]; 
}


--13)
gradoEP :: ExPol -> Int
gradoEP = \t -> case t of {
	Pol p -> maximum (map snd p);
	Der d -> gradoEP d;
	Eval a n -> gradoEP a;
	Sum x -> maximum (map gradoEP x);
	Prod x -> maximum (map gradoEP x);
}  
	
-- 14)	
calcEP :: ExPol -> Polinomio
calcEP = \t -> case t of {
    Pol p -> redPol p;
    Der d -> redPol (derPol (calcEP d));
    Eval a n -> redPol ([(evalPol (calcEP a) n, 0)]);
    Sum x -> sumList x;
    Prod x -> prodList x;
}

sumList :: [ExPol] -> Polinomio
sumList = \x -> case x of {
    [] -> [];
    y:ys -> redPol (sumPol (calcEP y) (sumList ys));
}

prodList :: [ExPol] -> Polinomio
prodList = \x -> case x of {
    [] -> [(1,0)];
    y:ys -> redPol (mulPol (calcEP y) (prodList ys));
}

--15)
resultado :: ExPol -> String
resultado = \t -> showPol (calcEP t);

