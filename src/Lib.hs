{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
  ( Education (..),
    Enviroment (..),
    Guns (..),
    Healthcare (..),
    Immigration (..),
    Positions,
    Topics,
    comparePositions,
    getQuestion,
    getTopics,
    topic,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Void

type TopicsList = '[Education, Enviroment, Guns, Healthcare, Immigration]

type Topics = Summed TopicsList

class Summable (xs :: [*]) where
  data Summed xs :: *

instance Summable '[] where
  data Summed '[] = SummedNil Void

instance Summable (a ': xs) where
  data Summed (a ': xs)
    = Here a
    | Elsewhere (Summed xs)

instance Show (Summed '[]) where
  show _ = ""

instance (Show a, Show (Summed xs)) => Show (Summed (a ': xs)) where
  show (Here a) = "Here " ++ show a
  show (Elsewhere a) = "Elsewhere " ++ show a

instance Eq (Summed '[]) where
  (==) _ _ = True

instance (Eq a, Eq (Summed xs)) => Eq (Summed (a ': xs)) where
  (==) (Here a) (Here b) = a == b
  (==) (Here _) _ = False
  (==) (Elsewhere a) (Elsewhere b) = (==) a b
  (==) (Elsewhere _) (Here _) = False

instance Ord (Summed '[]) where
  compare _ _ = EQ

instance (Ord a, Ord (Summed xs)) => Ord (Summed (a ': xs)) where
  compare (Here a) (Here b) = compare a b
  compare (Here _) _ = GT
  compare (Elsewhere a) (Elsewhere b) = compare a b
  compare (Elsewhere _) _ = LT

instance IsTopic (Summed '[]) where
  getQuestion _ = ""

instance (IsTopic a, IsTopic (Summed xs)) => IsTopic (Summed (a ': xs)) where
  getQuestion (Here x) = getQuestion x
  getQuestion (Elsewhere y) = getQuestion y

class Injectable (f :: *) (fs :: [*]) where
  inj :: f -> Summed fs

instance Injectable f (f ': fs) where
  inj = Here

instance {-# OVERLAPPABLE #-} Injectable a xs => Injectable a (b ': xs) where
  inj = Elsewhere . inj

class
  ( Summable xs,
    Injectable a xs
  ) =>
  (a :: *) :<: (xs :: [*])

instance
  ( Summable xs,
    Injectable a xs
  ) =>
  (a :<: xs)

class IsTopic a where
  getQuestion :: a -> String

topic :: (a :<: TopicsList) => a -> Topics
topic = inj

class IsTopicList a where
  getTopics :: [Topics]

instance IsTopicList '[] where
  getTopics = []

-- Example:
-- getTopics @'[Education, Enviroment, Healthcare]
instance (a :<: TopicsList, Bounded a, Enum a, IsTopicList xs) => IsTopicList (a ': xs) where
  getTopics = (topic @a <$> [minBound .. maxBound]) ++ getTopics @xs

data Education
  = TuitionFreePublicCollege
  | DebtReliefForStudentLoans
  | AffirmativeAction
  | UniversalChildCare
  | UniversalPreKindergarten
  | IncreaseFundingForPublicEducation
  deriving (Show, Eq, Ord, Bounded, Enum)

instance IsTopic Education where
  getQuestion TuitionFreePublicCollege = "Public college should be tuition free"
  getQuestion DebtReliefForStudentLoans = "The government should offer debt relief for student loans"
  getQuestion AffirmativeAction = "Affirmative action is a fundamentally good idea"
  getQuestion UniversalChildCare = "I am in favor of universal child care"
  getQuestion UniversalPreKindergarten = "I am in favor of universal pre-kindergarten"
  getQuestion IncreaseFundingForPublicEducation = "The government should increase funding for primary and secondary public education"

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

instance IsTopic Enviroment where
  getQuestion GreenNewDeal = "The Green New Deal is a good idea"
  getQuestion NoFossilFuelMoneyPledge = "It is important to me that my candidate has taken the No Fossil Fuel Money Pledge"
  getQuestion NuclearPowerToReduceEmissions = "Leveraging nuclear power is a good way for the US to reduce emissions"
  getQuestion CarbonTax = "Companies should be have to pay more taxes if they have higher carbon emissions"
  getQuestion ParisAgreement = "The US should rejoin the Paris Climate Agreement"
  getQuestion BanFracking = "Fracking should be banned in the US"
  getQuestion BanOffshoreDrilling = "Offshore drilling should be banned"
  getQuestion DeclareClimateChangeANationalEmergency = "Climate change should be declared a national emergency"

data Guns
  = UniversalBackgroundChecks
  | BanAssaultWeapons
  | GunBuyBack
  | RequireGunLicense
  deriving (Show, Eq, Ord, Bounded, Enum)

instance IsTopic Guns where
  getQuestion UniversalBackgroundChecks = "The law should require anyone trying to buy a gun to have a background check done of them"
  getQuestion BanAssaultWeapons = "Assault weapons should be banned"
  getQuestion GunBuyBack = "The government should implement a guy buy-back program"
  getQuestion RequireGunLicense = "All guns should require a license to own"

data Healthcare
  = SinglePayerSystem
  | PublicHealthInsurance
  | EliminatePrivateHealthInsurance
  | ImportPrescriptionDrugsFromCanada
  deriving (Show, Eq, Ord, Bounded, Enum)

instance IsTopic Healthcare where
  getQuestion SinglePayerSystem = "The US should have a single-payer healthcare system" -- Single-Payer Bill (H.R. 676)
  getQuestion PublicHealthInsurance = "The US should have a public health insurance option for those who want it"
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
    f (p, x) acc =
      if x == 0
        then 0
        else (acc +) . abs $ x - (lookupTopic p p2)
