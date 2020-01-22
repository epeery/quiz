{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Blog
  ( Education (..),
    Enviroment (..),
    Guns (..),
    Healthcare (..),
    Immigration (..),
    Positions,
    comparePositions,
    getQuestion,
    getTopics,
    topic,
  )
where

import Data.Aeson.Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Generics

type Topics = Education + Enviroment + Guns + Healthcare + Immigration

data a + b = InL a | InR b
  deriving (Show, Eq, Ord, Generic)

instance (FromJSON a, FromJSON b) => FromJSON (a + b)

instance (ToJSON a, ToJSON b) => ToJSON (a + b)

infixr 8 +

class a :<: b where
  inj :: a -> b

instance a :<: a where
  inj = id

instance a :<: (a + b) where
  inj = InL

instance {-# OVERLAPPABLE #-} (a :<: c) => a :<: (b + c) where
  inj = InR . inj

class IsTopicList a where
  getTopics :: [Topics]

instance IsTopicList '[] where
  getTopics = []

-- Example:
-- getTopics @'[Education, Enviroment, Healthcare]
instance (Injectable a, Bounded a, Enum a, IsTopicList xs) => IsTopicList (a ': xs) where
  getTopics = (inject @a <$> [minBound .. maxBound]) ++ getTopics @xs

class IsTopic a where
  getQuestion :: a -> String

class Injectable a where
  inject :: a -> Topics

instance (IsTopic a, IsTopic b) => IsTopic (a + b) where
  getQuestion (InL x) = getQuestion x
  getQuestion (InR y) = getQuestion y

topic :: (a :<: b) => a -> b
topic = inj

data Education
  = TuitionFreePublicCollege
  | DebtReliefForStudentLoans
  | AffirmativeAction
  | UniversalChildCare
  | UniversalPreKindergarten
  | IncreaseFundingForPublicEducation
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Injectable Education where
  inject = topic

instance IsTopic Education where
  getQuestion TuitionFreePublicCollege = "Public college should be tuition free"
  getQuestion DebtReliefForStudentLoans = "The government should offer debt relief for student loans"
  getQuestion AffirmativeAction = "Affirmative action is a fundamentally good idea"
  getQuestion UniversalChildCare = "I am in favor of universal child care"
  getQuestion UniversalPreKindergarten = "I am in favor of universal pre-kindergarten"
  getQuestion IncreaseFundingForPublicEducation = "The government should increase funding for public education"

data Enviroment
  = GreenNewDeal
  | NoFossilFuelMoneyPledge
  | NuclearPowerToReduceEmissions
  | CarbonTax
  | ParisAgreement
  | BanFracking
  | BanOffshoreDrilling
  | DeclareClimateChangeANationalEmergency
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Injectable Enviroment where
  inject = topic

instance IsTopic Enviroment where
  getQuestion GreenNewDeal = "The Green New deal is a good idea"
  getQuestion NoFossilFuelMoneyPledge = "It is important to me that my candidate has taken the No Fossil Fuel Money Pledge"
  getQuestion NuclearPowerToReduceEmissions = "Leveraging nuclear power is a good way for the US to reduce emissions"
  getQuestion CarbonTax = "Companies should be have to pay more taxes if they have higher carbon emissions"
  getQuestion ParisAgreement = "The US should rejoin the Paris Climate Agreement"
  getQuestion BanFracking = "All fracking in the US should be banned"
  getQuestion BanOffshoreDrilling = "Offshore drilling should be banned"
  getQuestion DeclareClimateChangeANationalEmergency = "Climate change should be declared a national emergency"

data Guns
  = UniversalBackgroundChecks
  | BanAssaultWeapons
  | GunBuyBack
  | RequireGunLicense
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Injectable Guns where
  inject = topic

instance IsTopic Guns where
  getQuestion UniversalBackgroundChecks = "The law should require background checks of anyone trying to buy a gun"
  getQuestion BanAssaultWeapons = "All assault weapons should be banned"
  getQuestion GunBuyBack = "The government should implement a guy buy-back program"
  getQuestion RequireGunLicense = "All guns should require a license to own"

data Healthcare
  = SinglePayerSystem
  | PublicHealthInsurance
  | EliminatePrivateHealthInsurance
  | ImportPrescriptionDrugsFromCanada
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Injectable Healthcare where
  inject = topic

instance IsTopic Healthcare where
  getQuestion SinglePayerSystem = "I support a single-payer healthcare system"
  getQuestion PublicHealthInsurance = "I am in favor of the US having a public health insurance option"
  getQuestion EliminatePrivateHealthInsurance = "Private health insurance should be eliminated"
  getQuestion ImportPrescriptionDrugsFromCanada = "The US should import some prescription drugs from Canada"

data Immigration
  = TrumpBorderWall
  | TrumpTravelBan
  | SupportDACA
  | AllowMoreVisaWorkers
  | DemilitarizeMexicoUSBorder
  | InvestInPortsOfEntry
  | AbolishICE
  | DecriminalizeIllegalImmigration
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Injectable Immigration where
  inject = topic

instance IsTopic Immigration where
  getQuestion TrumpBorderWall = "Trump's border wall is a good idea"
  getQuestion TrumpTravelBan = "Trump's travel ban was a good idea"
  getQuestion SupportDACA = "The US should support DACA"
  getQuestion AllowMoreVisaWorkers = "The US should allow more visa workers in"
  getQuestion DemilitarizeMexicoUSBorder = "The Mexico-US border should be demilitarized"
  getQuestion InvestInPortsOfEntry = "The US should invest in ports of entry"
  getQuestion AbolishICE = "ICE should be abolished"
  getQuestion DecriminalizeIllegalImmigration = "Illegal immigration shouldn't be a crime"

type Positions = Map Topics Float

lookupTopic :: Topics -> Positions -> Float
lookupTopic = M.findWithDefault 0

-- Temporary way to compare positions
-- Lower number == more similar
comparePositions :: Positions -> Positions -> Float
comparePositions p1 p2 = foldr f 0 (M.toList p1)
  where
    f (p, x) acc = (acc +) . abs $ x - (lookupTopic p p2)
