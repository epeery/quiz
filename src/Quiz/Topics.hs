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
    Info,
    Positions,
    Topics,
    comparePositions,
    getQuestion,
    getQuestionInfo,
    getTopic,
    getTopics,
    inject,
    percentageMatch,
    topic,
  )
where

import Data.Aeson.Types
import Data.List (filter)
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

  getQuestionInfo :: a -> Info

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

data Info = Info {header :: Text, info :: Text, source :: Text}
  deriving (Generic)

instance FromJSON Info

instance ToJSON Info

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

  getQuestionInfo TuitionFreePublicCollege =
    Info
      { header = "Tuition Free Public College",
        info = "Most Democrats have gotten behind the idea of some form of tuition-free or debt-free college, but they disagree about how much of the tab should be covered. Several candidates have called for making four years of public college free for students under a certain income threshold while others would cover only community college or technical school.",
        source = "https://www.politico.com/2020-election/candidates-views-on-the-issues/education-reform/free-college/"
      }
  getQuestionInfo DebtReliefForStudentLoans =
    Info
      { header = "Debt Relief For Student Loans",
        info = "",
        source = ""
      }
  getQuestionInfo AffirmativeAction =
    Info
      { header = "Affirmative Action",
        info = "",
        source = ""
      }
  getQuestionInfo UniversalChildCare =
    Info
      { header = "Universal Child Care",
        info = "",
        source = ""
      }
  getQuestionInfo UniversalPreKindergarten =
    Info
      { header = "Universal Pre-Kindergarten",
        info = "",
        source = ""
      }
  getQuestionInfo IncreaseFundingForPublicEducation =
    Info
      { header = "Increasing Funding For Public Education",
        info = "",
        source = ""
      }

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

  getQuestionInfo GreenNewDeal =
    Info
      { header = "The Green New Deal",
        info = "",
        source = ""
      }
  getQuestionInfo NoFossilFuelMoneyPledge =
    Info
      { header = "The No Fossil Fuel Money Pledge",
        info = "I pledge not to take contributions over $200 from oil, gas, and coal industry executives, lobbyists, and PACs and instead prioritize the health of our families, climate, and democracy over fossil fuel industry profits.",
        source = "http://nofossilfuelmoney.org/"
      }
  getQuestionInfo NuclearPowerToReduceEmissions =
    Info
      { header = "Nuclear Power To Reduce Emissions",
        info = "",
        source = ""
      }
  getQuestionInfo CarbonTax =
    Info
      { header = "The Carbon Tax",
        info = "",
        source = ""
      }
  getQuestionInfo ParisAgreement =
    Info
      { header = "The Paris Climate Agreement",
        info = "",
        source = ""
      }
  getQuestionInfo BanFracking =
    Info
      { header = "Fracking",
        info = "",
        source = ""
      }
  getQuestionInfo BanOffshoreDrilling =
    Info
      { header = "Offshore Drilling",
        info = "",
        source = ""
      }
  getQuestionInfo DeclareClimateChangeANationalEmergency =
    Info
      { header = "Declaring Climate Change as a National Emergency",
        info = "",
        source = ""
      }

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

  getQuestionInfo UniversalBackgroundChecks =
    Info
      { header = "Universal Background Checks",
        info = "",
        source = ""
      }
  getQuestionInfo BanAssaultWeapons =
    Info
      { header = "Assault Weapons",
        info = "",
        source = ""
      }
  getQuestionInfo GunBuyBack =
    Info
      { header = "Gun Buy Back",
        info = "",
        source = ""
      }
  getQuestionInfo RequireGunLicense =
    Info
      { header = "Gun Licenses",
        info = "",
        source = ""
      }

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

  getQuestionInfo SinglePayerSystem =
    Info
      { header = "Single Payer Healthcare",
        info = "",
        source = ""
      }
  getQuestionInfo PublicHealthInsurance =
    Info
      { header = "Public Health Insurance",
        info = "",
        source = ""
      }
  getQuestionInfo EliminatePrivateHealthInsurance =
    Info
      { header = "Private Health Insurance",
        info = "",
        source = ""
      }
  getQuestionInfo ImportPrescriptionDrugsFromCanada =
    Info
      { header = "Importing Prescription Drugs from Canada",
        info = "",
        source = ""
      }

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

  getQuestion TrumpBorderWall = "The border wall is a good idea"
  getQuestion TrumpTravelBan = "Trump's travel ban was a good idea"
  getQuestion SupportDACA = "The US should support DACA"
  getQuestion AllowMoreVisaWorkers = "The US should allow more visa workers in"
  getQuestion DemilitarizeMexicoUSBorder = "The Mexico-US border should be demilitarized"
  getQuestion InvestInPortsOfEntry = "The US should invest in ports of entry"
  getQuestion AbolishICE = "ICE should be abolished"
  getQuestion DecriminalizeIllegalImmigration = "Illegal immigration shouldn't be a crime"

  getQuestionInfo TrumpBorderWall =
    Info
      { header = "The Border Wall",
        info = "",
        source = ""
      }
  getQuestionInfo TrumpTravelBan =
    Info
      { header = "Trump's Travel Ban",
        info = "",
        source = ""
      }
  getQuestionInfo SupportDACA =
    Info
      { header = "DACA",
        info = "",
        source = ""
      }
  getQuestionInfo AllowMoreVisaWorkers =
    Info
      { header = "Visa Workers",
        info = "",
        source = ""
      }
  getQuestionInfo DemilitarizeMexicoUSBorder =
    Info
      { header = "Demilitarizing the Mexico-US Border",
        info = "",
        source = ""
      }
  getQuestionInfo InvestInPortsOfEntry =
    Info
      { header = "Ports of Entry",
        info = "",
        source = ""
      }
  getQuestionInfo AbolishICE =
    Info
      { header = "ICE",
        info = "",
        source = ""
      }
  getQuestionInfo DecriminalizeIllegalImmigration =
    Info
      { header = "Illegal Immigration",
        info = "",
        source = ""
      }

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
    p1' = filter (\(_, n) -> n /= 0) $ M.toList p1
    l = length p1'
    -- Should in reality be 2 * l but 1.5 makes the results more interesting
    highest = 1.5 * (fromIntegral l)
    result = comparePositions' p1' p2
    result' = if result > highest then highest else result
