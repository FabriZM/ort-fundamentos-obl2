{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs#-}

module Polinomios where

type Monomio = (Int, Int)
type Polinomio = [Monomio]

-- ======================
-- POLINOMIOS
-- ======================

--1)
agregarMon :: Monomio -> Polinomio -> Polinomio
agregarMon = \m -> \p -> case (fst m == 0) of {
    True -> p;
    False -> case p of {
        [] -> [m];
        x:xs -> case (snd m == snd x) of {
            True -> case  (fst m + fst x == 0) of {
                True -> xs;
                False -> (fst m + fst x, snd x) : xs;
            };
            False -> case (snd m > snd x) of {
                True -> m : p;
                False -> x : (agregarMon m xs);
            }
        }
    }    
}


--2)
redPol :: Polinomio -> Polinomio
redPol = \p -> case p of {
    [] -> [];
    x:xs -> agregarMon x (redPol xs);
}

--3)
sumPol :: Polinomio -> Polinomio -> Polinomio
sumPol = \p1 -> \p2 -> case p1 of {
    [] -> p2;
    x:xs -> case p2 of {
        [] -> p1;
        y:ys -> case (snd x == snd y) of {
            True -> agregarMon (fst x + fst y, snd x) (sumPol xs ys);
            False -> case (snd x > snd y) of {
                True -> x : sumPol xs p2;
                False -> y : sumPol p1 ys;
            }
        }
    }
}

--4)
mulPol :: Polinomio -> Polinomio -> Polinomio
mulPol = \p1 -> \p2 -> case p1 of {
    [] -> [];
    x:xs -> case p2 of {
        [] -> [];
        y:ys -> sumPol (zip (map(* fst x) (map fst p2)) (map(+ snd x) (map snd p2))) (mulPol xs p2);
    }
}

--5)
derPol :: Polinomio -> Polinomio
derPol = \p -> case p of {
    [] -> [];
    x:xs -> case (snd x == 0) of {
        True -> [];
        False -> case (fst x * snd x == 0) of {
            True -> derPol xs;
            False -> (fst x * snd x, snd x - 1) : derPol xs;
        } 
    }
}

--6)
evalPol :: Polinomio -> Int -> Int
evalPol = \p -> \n -> case (n == 0) of {
    True -> case (snd (last p) == 0) of {
        True -> fst (last p);
        False -> 0; 
    };
    False -> case p of {
        [] -> 0;
        x:xs -> fst x * (n ^ snd x) + evalPol xs n;
    }
}

--7)
gradoPol::Polinomio -> Int
gradoPol = \p -> case p of {
    [] -> 0;
    x:xs -> snd x;
}
																	
																	
-- ======================
-- SHOW
-- ======================

--8)
showMon :: Monomio -> String
showMon = \(a,b) -> case b of {
    0 -> case a of {
        0 -> "";
        _ -> show a;
    };
    1 -> case a of {
        0 -> "";
        1 -> "x";
        (-1) -> "-x";
        _ -> show a ++ "x";
    };
    _ -> case a of {
        0 -> "";
        1 -> "x^" ++ show b;
        (-1) -> "-x^" ++ show b;
        _ -> show a ++ "x^" ++ show b;
    }
}

--9)
showPol :: Polinomio -> String
showPol = \p -> case p of {
    [] -> "";
    x:xs -> case xs of {
        [] -> showMon x;
        y:ys -> case (fst x > 0) of {
            True -> case (fst y > 0) of {
                True -> showMon x ++ "+" ++ showPol xs;
                False -> showMon x ++ showPol xs;
            };
            False -> case (fst y > 0) of {
                True -> "-" ++ showMon (((fst x) * (-1)), snd x) ++ "+" ++ showPol xs;
                False -> "-" ++ showMon (((fst x) * (-1)), snd x) ++ showPol xs;
            }
        }
    }
} 
