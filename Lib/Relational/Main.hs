{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where

--import Data.Char
--import Numeric
--import Control.Monad.Except hiding (MonadIO(..),join)

--import Numeric
import Lib.Monad.MonadIO
import Lib.DBC.Types
import Lib.DBC.HIODBC
--import Lib.TIR.Peano as Peano
import Lib.TIR.HList as HList
import Lib.TIR.HTypeGHC
import Lib.TIR.HRecord
--import Lib.DBC.Types
--import Lib.DBC.HSQL as SQL
import Lib.Relational.Types as SQL
import Lib.Relational.HSQL

------------------------------------------------------------------------------
-- import the database schema

import Lib.Relational.FamDb

------------------------------------------------------------------------------

main :: IO ()
main = dbConnectWith (odbcConnect (odbcConnection {odbcDsn = "testdb"})) famdb sqlMain 
                                                                                                                            
sqlMain :: Query ()
sqlMain = do
    moveContaminatedAnimal Cow BSE (DFarmId 10)
    -- closeCursor
    testBreed

------------------------------------------------------------------------------
-- beginnings of an SQL command

{-
UPDATE Animal
   SET Animal.location=42
 WHERE Animal.animalid IN
         SELECT Contaminated.animal
           FROM Contaminated,Animal
          WHERE Animal.type='cow'
           AND Contaminated.type='BSE'
           AND Animal.animalid=Contaminated.animal;
-}

moveContaminatedAnimal :: DAnimalType -> DCntdType -> DFarmId -> Query ()
moveContaminatedAnimal animalType contaminationType newLocation = do
    a1 <- table animalTable
    a2 <- restrict a1 (\r -> r!AnimalType `SQL.eq` animalType)
    c1 <- table contaminatedTable
    c2 <- restrict c1 (\r -> r!CntdType `SQL.eq` contaminationType)
    j1 <- join SqlInnerJoin a2 c2 (\r -> r!AnimalId `SQL.eq` r!CntdAnimal) 
    j2 <- project j1 (CntdAnimal .*. HNil)
    doUpdate animalTable
        (\_ -> AnimalLocation .=. toSqlType newLocation .*. HNil)
        (\r -> r!AnimalId `SQL.elem` relation j2)

    {-
    j3 <- limit j2 Peano.one
    (cols,rows) <- doSelect j3 -- run sub select so we can see whats happening
    ioPrint cols
    showRows rows
    -- doInsertR j2 animalTable (AnimalId .*. HNil)
    

    doInsertC animalTable (AnimalLocation .=. toSqlType 77 .*. HNil)

    r1 <- table animalTable
    r2 <- restrict r1 (\r -> r!AnimalLocation `SQL.eq` toSqlType 77)
    r3 <- order r2 (\r -> r!AnimalId)
    r4 <- union r3 r3
    result <- doSelect r4
    ioPrint result
    let f = \r -> case r!AnimalLocation of
            Just l -> ioPutStrLn $ (showString "Location: " . showInt l) ""
            _ -> ioPutStrLn "Location: <NULL>"      
        in mapM f result
    closeCursor
    doDelete animalTable (\r -> r!AnimalLocation `SQL.eq` toSqlType 77)
    result' <- doSelect r3
    ioPrint result'
    return ()
    -}

selectBreed :: DAnimalType -> Query [
    AnimalId :=: Maybe DAnimalId :*: 
    AnimalName :=: Maybe DAnimalName :*:
    HNil]
selectBreed b = do
    r1 <- table animalTable
    r2 <- restrict r1 (\r -> r!AnimalType `SQL.eq` b)
    r3 <- project r2 (AnimalId .*. AnimalName .*. HNil)
    doSelect r3

testBreed :: Query ()
testBreed = do
    results <- selectBreed Cow
    _ <- mapM (\r -> do
        case r!AnimalName of
            Just (DAnimalName l) -> ioPutStrLn $ (showString "Name: " . showString l) ""
            _ -> ioPutStrLn "Name: <NULL>"
        ) results
    return ()
