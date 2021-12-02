import Data.Either
-- Monads
{--- Example of another Monad: Maybe
-- from the Prelude:
 instance Monad Maybe where
    return x       = Just x

-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing  >>= f = Nothing
    (Just x) >>= f = f x

-- instance Monad Either where
   return x = Right x

-- (>>=) :: Either a -> (a -> Either b) -> Either b
   (Left x)  >>= _ = Left x -- do nothing with the function, return as given
   (Right x) >>= f = f x -- since f returns a Either b

-----------------------------------------}

type CarReg = String ; type PNr = String  
type Name = String   ; type Address = String

carRegister :: [(CarReg,PNr)]
carRegister
 = [("FYN 433","850219-1234"),
    ("GYN 434","850219-1234"),
    ("JBD 007","750408-0909")]

nameRegister :: [(PNr,Name)]
nameRegister 
 = [("750408-0909","Dave")
   ,("850219-1234","Bob") 
   ,("890929-C234","Pierre")]             

addressRegister :: [((Name,PNr),Address)]
addressRegister = 
  [(("Dave","750408-0909"),"42 Streetgatan\n Askim")
  ,(("Bob","850219-1234") ,"1 Chalmers Av\n Gothenburg") ]

billingAddress :: CarReg -> Either String (Name, Address)
-- given a registration number, 
-- returns the name and address of owner, if defined. 

billingAddress car = case lookup car carRegister of
               Nothing -> Left "Could not find car"
               Just pn -> case lookup pn nameRegister of
                               Nothing -> Left "Could not find name"
                               Just name -> 
                                  case lookup (name,pn) addressRegister of      
                                       Nothing -> Left "Could not find address"
                                       Just address -> Right (name,address)

-- Monadic style

billingAddress' :: CarReg -> Maybe (Name, Address)
billingAddress' car = 
  do
     pn      <- lookup car carRegister
     name    <- lookup pn nameRegister
     address <- lookup (name,pn) addressRegister
     return (name,address)

