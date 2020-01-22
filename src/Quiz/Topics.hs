{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Quiz.Topics
  ( Education (..),
    Enviroment (..),
    Guns (..),
    Healthcare (..),
    Immigration (..),
    Positions,
    Topics,
    comparePositions,
    getQuestion,
    getQuestionInfo,
    getTopic,
    getTopics,
    percentageMatch,
    questions,
    inject,
    topic,
  )
where

import Data.Aeson.Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import GHC.Generics

type Topics = Education + Enviroment + Guns + Healthcare + Immigration

data a + b = InL a | InR b
  deriving (Show, Eq, Ord, Generic)

infixr 8 +

instance (ToJSON a, ToJSON b) => ToJSON (a + b)

instance (FromJSON a, FromJSON b) => FromJSON (a + b)

instance (ToJSON a, ToJSON b) => ToJSONKey (a + b)

instance (FromJSON a, FromJSON b) => FromJSONKey (a + b)

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

  getQuestion :: a -> Text

  getQuestionInfo :: a -> Text

  getTopic :: a -> Text

class Injectable a where
  inject :: a -> Topics

instance (IsTopic a, IsTopic b) => IsTopic (a + b) where

  getQuestion (InL x) = getQuestion x
  getQuestion (InR y) = getQuestion y

  getQuestionInfo (InL x) = getQuestionInfo x
  getQuestionInfo (InR y) = getQuestionInfo y

  getTopic (InL x) = getTopic x
  getTopic (InR y) = getTopic y

topic :: (a :<: b) => a -> b
topic = inj

data Education
  = TuitionFreePublicCollege
  | DebtReliefForStudentLoans
  | AffirmativeAction
  | UniversalChildCare
  | UniversalPreKindergarten
  | IncreaseFundingForPublicEducation
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance IsTopic Education where

  getQuestion TuitionFreePublicCollege = "Public college should be tuition free"
  getQuestion DebtReliefForStudentLoans = "The government should offer debt relief for student loans"
  getQuestion AffirmativeAction = "Affirmative action is a fundamentally good idea"
  getQuestion UniversalChildCare = "I am in favor of universal child care"
  getQuestion UniversalPreKindergarten = "I am in favor of universal pre-kindergarten"
  getQuestion IncreaseFundingForPublicEducation = "The government should increase funding for primary and secondary public education"

  getQuestionInfo TuitionFreePublicCollege = "TODO: add information about questions"
  getQuestionInfo DebtReliefForStudentLoans = "TODO: add information about questions"
  getQuestionInfo AffirmativeAction = "TODO: add information about questions"
  getQuestionInfo UniversalChildCare = "TODO: add information about questions"
  getQuestionInfo UniversalPreKindergarten = "TODO: add information about questions"
  getQuestionInfo IncreaseFundingForPublicEducation = "TODO: add information about questions"

  getTopic = const "Education"

instance Injectable Education where
  inject = topic

instance ToJSON Education

instance FromJSON Education

data Enviroment
  = GreenNewDeal
  | NoFossilFuelMoneyPledge
  | NuclearPowerToReduceEmissions
  | CarbonTax
  | ParisAgreement
  | BanFracking
  | BanOffshoreDrilling
  | DeclareClimateChangeANationalEmergency
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance IsTopic Enviroment where

  getQuestion GreenNewDeal = "The Green New Deal is a good idea"
  getQuestion NoFossilFuelMoneyPledge = "It is important to me that my candidate has taken the No Fossil Fuel Money Pledge"
  getQuestion NuclearPowerToReduceEmissions = "Leveraging nuclear power is a good way for the US to reduce emissions"
  getQuestion CarbonTax = "Companies should be have to pay more taxes if they have higher carbon emissions"
  getQuestion ParisAgreement = "The US should rejoin the Paris Climate Agreement"
  getQuestion BanFracking = "Fracking should be banned in the US"
  getQuestion BanOffshoreDrilling = "Offshore drilling should be banned"
  getQuestion DeclareClimateChangeANationalEmergency = "Climate change should be declared a national emergency"

  getQuestionInfo GreenNewDeal = "TODO: add information about questions"
  getQuestionInfo NoFossilFuelMoneyPledge = "TODO: add information about questions"
  getQuestionInfo NuclearPowerToReduceEmissions = "TODO: add information about questions"
  getQuestionInfo CarbonTax = "TODO: add information about questions"
  getQuestionInfo ParisAgreement = "TODO: add information about questions"
  getQuestionInfo BanFracking = "TODO: add information about questions"
  getQuestionInfo BanOffshoreDrilling = "TODO: add information about questions"
  getQuestionInfo DeclareClimateChangeANationalEmergency = "TODO: add information about questions"

  getTopic = const "Enviroment"

instance Injectable Enviroment where
  inject = topic

instance ToJSON Enviroment

instance FromJSON Enviroment

data Guns
  = UniversalBackgroundChecks
  | BanAssaultWeapons
  | GunBuyBack
  | RequireGunLicense
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance IsTopic Guns where

  getQuestion UniversalBackgroundChecks = "The law should require anyone trying to buy a gun to have a background check done of them"
  getQuestion BanAssaultWeapons = "Assault weapons should be banned"
  getQuestion GunBuyBack = "The government should implement a gun buy-back program"
  getQuestion RequireGunLicense = "All guns should require a license to own"

  getQuestionInfo UniversalBackgroundChecks = "TODO: add information about questions"
  getQuestionInfo BanAssaultWeapons = "TODO: add information about questions"
  getQuestionInfo GunBuyBack = "TODO: add information about questions"
  getQuestionInfo RequireGunLicense = "TODO: add information about questions"

  getTopic = const "Guns"

instance Injectable Guns where
  inject = topic

instance ToJSON Guns

instance FromJSON Guns

data Healthcare
  = SinglePayerSystem
  | PublicHealthInsurance
  | EliminatePrivateHealthInsurance
  | ImportPrescriptionDrugsFromCanada
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance IsTopic Healthcare where

  getQuestion SinglePayerSystem = "The US should have a single-payer healthcare system" -- Single-Payer Bill (H.R. 676)
  getQuestion PublicHealthInsurance = "The US should have a public health insurance option for those who want it"
  getQuestion EliminatePrivateHealthInsurance = "Private health insurance should be eliminated"
  getQuestion ImportPrescriptionDrugsFromCanada = "The US should import some prescription drugs from Canada"

  getQuestionInfo SinglePayerSystem = "TODO: add information about questions" -- Single-Payer Bill (H.R. 676)
  getQuestionInfo PublicHealthInsurance = "TODO: add information about questions"
  getQuestionInfo EliminatePrivateHealthInsurance = "TODO: add information about questions"
  getQuestionInfo ImportPrescriptionDrugsFromCanada = "TODO: add information about questions"

  getTopic = const "Healthcare"

instance Injectable Healthcare where
  inject = topic

instance ToJSON Healthcare

instance FromJSON Healthcare

data Immigration
  = TrumpBorderWall
  | TrumpTravelBan
  | SupportDACA
  | AllowMoreVisaWorkers
  | DemilitarizeMexicoUSBorder
  | InvestInPortsOfEntry
  | AbolishICE
  | DecriminalizeIllegalImmigration
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance IsTopic Immigration where

  getQuestion TrumpBorderWall = "Trump's border wall is a good idea"
  getQuestion TrumpTravelBan = "Trump's travel ban was a good idea"
  getQuestion SupportDACA = "The US should support DACA"
  getQuestion AllowMoreVisaWorkers = "The US should allow more visa workers in"
  getQuestion DemilitarizeMexicoUSBorder = "The Mexico-US border should be demilitarized"
  getQuestion InvestInPortsOfEntry = "The US should invest in ports of entry"
  getQuestion AbolishICE = "ICE should be abolished"
  getQuestion DecriminalizeIllegalImmigration = "Illegal immigration shouldn't be a crime"

  getQuestionInfo TrumpBorderWall = "TODO: add information about questions"
  getQuestionInfo TrumpTravelBan = "TODO: add information about questions"
  getQuestionInfo SupportDACA = "TODO: add information about questions"
  getQuestionInfo AllowMoreVisaWorkers = "TODO: add information about questions"
  getQuestionInfo DemilitarizeMexicoUSBorder = "TODO: add information about questions"
  getQuestionInfo InvestInPortsOfEntry = "TODO: add information about questions"
  getQuestionInfo AbolishICE = "TODO: add information about questions"
  getQuestionInfo DecriminalizeIllegalImmigration = "TODO: add information about questions"

  getTopic = const "Immigration"

instance Injectable Immigration where
  inject = topic

instance ToJSON Immigration

instance FromJSON Immigration

type Positions = Map Topics Double

lookupTopic :: Topics -> Positions -> Double
lookupTopic = M.findWithDefault 0

-- Temporary way to compare positions
-- Lower number == more similar
comparePositions :: Positions -> Positions -> Double
comparePositions p1 = comparePositions' (M.toList p1)

comparePositions' :: [(Topics, Double)] -> Positions -> Double
comparePositions' p1 p2 = foldr f 0 p1
  where
    f (p, x) acc =
      if x == 0
        then acc
        else (acc +) . abs $ x - (lookupTopic p p2)

pos1 :: Positions
pos1 = M.fromList [(topic TrumpBorderWall, 1)]

pos2 :: Positions
pos2 = M.fromList [(topic TrumpBorderWall, -1)]

percentageMatch :: Positions -> Positions -> Double
percentageMatch p1 p2 = if highest == 0 then 0 else abs (result' - highest) / highest
  where
    p1' = M.toList p1
    l = length p1'
    -- Should in reality be 2 * l but 1.5 makes the results more interesting
    highest = 1.5 * (fromIntegral l)
    result = comparePositions' p1' p2
    result' = if result > highest then highest else result

questions :: [(Text, Topics)]
questions = (\f -> (getQuestion f, f)) <$> getTopics @'[Education, Enviroment, Guns, Healthcare, Immigration]
