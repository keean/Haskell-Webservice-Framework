{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

module Lib.Relational.FamDb where

import Char
import Lib.ODBC.Types
import Lib.TIR.HList
import Lib.TIR.HTypeGHC
import Lib.TIR.HRecord
import Lib.Relational.Types as SQL

-------------------------------------------------------------------------------
-- Foot and Mouth Database

famdb :: (FarmerTable:*:FarmTable:*:AnimalTable:*:ContaminatedTable:*: HNil)
famdb = (farmerTable.*.farmTable.*.animalTable.*.contaminatedTable.*.HNil)

-------------------------------------------------------------------------------
-- Domains

newtype DFarmerId = DFarmerId Int deriving (Show,Eq,ToSqlType SqlInteger,FromSqlType SqlInteger)
newtype DFarmerName = DFarmerName String deriving (Show,Eq,ToSqlType SqlVarchar,FromSqlType SqlVarchar)
newtype DFarmId = DFarmId Int deriving (Show,Eq,ToSqlType SqlInteger,FromSqlType SqlInteger)
newtype DFarmName = DFarmName String deriving (Show,Eq,ToSqlType SqlVarchar,FromSqlType SqlVarchar)
newtype DFarmCounty = DFarmCounty String deriving (Show,Eq,ToSqlType SqlVarchar,FromSqlType SqlVarchar)
newtype DAnimalId = DAnimalId Int deriving (Show,Eq,ToSqlType SqlInteger,FromSqlType SqlInteger)
newtype DAnimalName = DAnimalName String deriving (Show,Eq,ToSqlType SqlVarchar,FromSqlType SqlVarchar)
data DAnimalType = Cow | Sheep deriving (Show,Eq)
newtype DAnimalPrice = DAnimalPrice Float deriving (Show,Eq,ToSqlType SqlNumeric,FromSqlType SqlNumeric)
data DCntdType = BSE | FM deriving (Show,Eq)

instance FromSqlType SqlVarchar DAnimalType where
   fromSqlType _ s = case (map toLower s) of
      "cow" -> Just Cow
      "sheep" -> Just Sheep
      _ -> Nothing
                                                                                                                     
instance ToSqlType SqlVarchar DAnimalType where
   toSqlType Cow = SqlTyped (SqlExpressionConst $ sqlShow "cow" "")
   toSqlType Sheep = SqlTyped (SqlExpressionConst $ sqlShow "sheep" "")

instance FromSqlType SqlVarchar DCntdType where
	fromSqlType _ s = case (map toLower s) of
		"bse" -> Just BSE
		"fm" -> Just FM
		_ -> Nothing

instance ToSqlType SqlVarchar DCntdType where
	toSqlType BSE = SqlTyped (SqlExpressionConst $ sqlShow "BSE" "")
	toSqlType FM = SqlTyped (SqlExpressionConst $ sqlShow "FM" "")

-------------------------------------------------------------------------------
-- Farmer table

data FarmerId = FarmerId deriving Show
data FarmerName = FarmerName deriving Show

type FarmerTable = Table (
	FarmerId :=: Attribute DFarmerId SqlInteger :*:
	FarmerName :=: Attribute DFarmerName SqlVarchar :*:
	HNil)	

farmerTable :: FarmerTable
farmerTable =  newTable "Farmer" (
	FarmerId .=. Attribute (attr { attrName="farmerid", attrType="SERIAL" }) .*.
	FarmerName .=. Attribute (attr { attrName="name", attrSize=20 }) .*.
	HNil)

-------------------------------------------------------------------------------
-- Farm table

data FarmId = FarmId deriving Show
data FarmName = FarmName deriving Show
data FarmCounty = FarmCounty deriving Show
data FarmOwner = FarmOwner deriving Show

type FarmTable = Table (
	FarmId :=: Attribute DFarmId SqlInteger :*:
	FarmName :=: Attribute DFarmName SqlVarchar :*:
	FarmCounty :=: Attribute DFarmCounty SqlVarchar :*:
	FarmOwner :=: Attribute DFarmerId SqlInteger :*:
	HNil)

farmTable :: FarmTable
farmTable = newTable "Farm" (
	FarmId .=. Attribute (attr { attrName="farmid", attrType="SERIAL" }) .*.
	FarmName .=. Attribute (attr { attrName="farmname", attrSize=20 }) .*.
	FarmCounty .=. Attribute (attr { attrName="county", attrSize=15 }) .*.
	FarmOwner .=. Attribute (attr { attrName="owner" }) .*.
	HNil)

------------------------------------------------------------------------------
-- Animal table

data AnimalId = AnimalId deriving Show
data AnimalName = AnimalName deriving Show
data AnimalType = AnimalType deriving Show
data AnimalPrice = AnimalPrice deriving Show
data AnimalLocation = AnimalLocation deriving Show

type AnimalTable = Table (
	AnimalId :=: Attribute DAnimalId SqlInteger :*:
	AnimalName :=: Attribute DAnimalName SqlVarchar :*:
	AnimalType :=:	Attribute DAnimalType SqlVarchar :*:
	AnimalPrice :=: Attribute DAnimalPrice SqlNumeric :*:
	AnimalLocation :=: Attribute DFarmId SqlInteger :*:
	HNil)

animalTable :: AnimalTable
animalTable = newTable "Animal" (
	AnimalId .=. Attribute (attr { attrName="animalid", attrType="SERIAL" }) .*.
	AnimalName .=. Attribute (attr { attrName="name", attrSize=15 }) .*.
	AnimalType .=. Attribute (attr { attrName="type", attrSize=10 }) .*.
	AnimalPrice .=. Attribute (attr { attrName="price", attrPrecision=8, attrScale=2 }) .*. -- NUMERIC(8,2)?
	AnimalLocation .=. Attribute (attr { attrName="location" }) .*. HNil)

------------------------------------------------------------------------------
-- Contaminated table

data CntdFarm = CntdFarm deriving Show
data CntdAnimal = CntdAnimal deriving Show
data CntdType = CntdType deriving Show

type ContaminatedTable = Table (
	CntdFarm :=: Attribute DFarmId SqlInteger :*:
	CntdAnimal :=: Attribute DAnimalId SqlInteger :*:
	CntdType :=: Attribute DCntdType SqlVarchar :*:
	HNil)

contaminatedTable :: ContaminatedTable
contaminatedTable = newTable "Contaminated" (
	CntdFarm .=. Attribute (attr { attrName="farm" }) .*.
	CntdAnimal .=. Attribute (attr { attrName="animal" }) .*.
	CntdType .=. Attribute (attr { attrName="type", attrSize=4 }) .*.
	HNil)

