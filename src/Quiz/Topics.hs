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
    Question,
    Positions,
    Topics,
    comparePositions,
    getQuestion,
    inject,
    percentageMatch,
    questions,
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
  getQuestion :: a -> Question

class Injectable a where
  inject :: a -> Topics

instance (IsTopic a, IsTopic b) => IsTopic (a + b) where
  getQuestion (InL x) = getQuestion x
  getQuestion (InR y) = getQuestion y

topic :: (a :<: b) => a -> b
topic = inj

data Question
  = Question
      { question :: Text,
        header :: Text,
        info :: Text,
        source :: Text,
        questionTopic :: Text,
        qId :: Topics
      }
  deriving (Generic)

instance FromJSON Question

instance ToJSON Question

data Education
  = TuitionFreePublicCollege
  | DebtReliefForStudentLoans
  | AffirmativeAction
  | UniversalChildCare
  | UniversalPreKindergarten
  | IncreaseFundingForPublicEducation
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance IsTopic Education where
  getQuestion TuitionFreePublicCollege =
    Question
      { header = "Tuition Free Public College",
        info = "Most Democrats have gotten behind the idea of some form of tuition-free or debt-free college, but they disagree about how much of the tab should be covered. Several candidates have called for making four years of public college free for students under a certain income threshold while others would cover only community college or technical school.",
        source = "https://www.politico.com/2020-election/candidates-views-on-the-issues/education-reform/free-college/",
        questionTopic = "Education",
        question = "Public college should be tuition free",
        qId = topic TuitionFreePublicCollege
      }
  getQuestion DebtReliefForStudentLoans =
    Question
      { header = "Debt Relief For Student Loans",
        info = "",
        source = "",
        questionTopic = "Education",
        question = "The government should offer debt relief for student loans",
        qId = topic DebtReliefForStudentLoans
      }
  getQuestion AffirmativeAction =
    Question
      { header = "Affirmative Action",
        info = "",
        source = "",
        questionTopic = "Education",
        question = "Affirmative action is a fundamentally good idea",
        qId = topic AffirmativeAction
      }
  getQuestion UniversalChildCare =
    Question
      { header = "Universal Child Care",
        info = "",
        source = "",
        questionTopic = "Education",
        question = "I am in favor of universal child care",
        qId = topic UniversalChildCare
      }
  getQuestion UniversalPreKindergarten =
    Question
      { header = "Universal Pre-Kindergarten",
        info = "",
        source = "",
        questionTopic = "Education",
        question = "I am in favor of universal pre-kindergarten",
        qId = topic UniversalPreKindergarten
      }
  getQuestion IncreaseFundingForPublicEducation =
    Question
      { header = "Increasing Funding For Public Education",
        info = "",
        source = "",
        questionTopic = "Education",
        question = "The government should increase funding for primary and secondary public education",
        qId = topic IncreaseFundingForPublicEducation
      }

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
  getQuestion GreenNewDeal =
    Question
      { header = "The Green New Deal",
        info = "",
        source = "",
        questionTopic = "Enviroment",
        question = "The Green New Deal is a good idea",
        qId = topic GreenNewDeal
      }
  getQuestion NoFossilFuelMoneyPledge =
    Question
      { header = "The No Fossil Fuel Money Pledge",
        info = "I pledge not to take contributions over $200 from oil, gas, and coal industry executives, lobbyists, and PACs and instead prioritize the health of our families, climate, and democracy over fossil fuel industry profits.",
        source = "http://nofossilfuelmoney.org/",
        questionTopic = "Enviroment",
        question = "It is important to me that my candidate has taken the No Fossil Fuel Money Pledge",
        qId = topic NoFossilFuelMoneyPledge
      }
  getQuestion NuclearPowerToReduceEmissions =
    Question
      { header = "Nuclear Power To Reduce Emissions",
        info = "",
        source = "",
        questionTopic = "Enviroment",
        question = "Leveraging nuclear power is a good way for the US to reduce emissions",
        qId = topic NuclearPowerToReduceEmissions
      }
  getQuestion CarbonTax =
    Question
      { header = "The Carbon Tax",
        info = "",
        source = "",
        questionTopic = "Enviroment",
        question = "Companies should be have to pay more taxes if they have higher carbon emissions",
        qId = topic CarbonTax
      }
  getQuestion ParisAgreement =
    Question
      { header = "The Paris Climate Agreement",
        info = "",
        source = "",
        questionTopic = "Enviroment",
        question = "The US should rejoin the Paris Climate Agreement",
        qId = topic ParisAgreement
      }
  getQuestion BanFracking =
    Question
      { header = "Fracking",
        info = "",
        source = "",
        questionTopic = "Enviroment",
        question = "Fracking should be banned in the US",
        qId = topic BanFracking
      }
  getQuestion BanOffshoreDrilling =
    Question
      { header = "Offshore Drilling",
        info = "",
        source = "",
        questionTopic = "Enviroment",
        question = "Offshore drilling should be banned",
        qId = topic BanOffshoreDrilling
      }
  getQuestion DeclareClimateChangeANationalEmergency =
    Question
      { header = "Declaring Climate Change as a National Emergency",
        info = "",
        source = "",
        questionTopic = "Enviroment",
        question = "Climate change should be declared a national emergency",
        qId = topic DeclareClimateChangeANationalEmergency
      }

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
  getQuestion UniversalBackgroundChecks =
    Question
      { header = "Universal Background Checks",
        info = "",
        source = "",
        questionTopic = "Guns",
        question = "The law should require anyone trying to buy a gun to have a background check done of them",
        qId = topic UniversalBackgroundChecks
      }
  getQuestion BanAssaultWeapons =
    Question
      { header = "Assault Weapons",
        info = "",
        source = "",
        questionTopic = "Guns",
        question = "Assault weapons should be banned",
        qId = topic BanAssaultWeapons
      }
  getQuestion GunBuyBack =
    Question
      { header = "Gun Buy Back",
        info = "",
        source = "",
        questionTopic = "Guns",
        question = "The government should implement a gun buy-back program",
        qId = topic GunBuyBack
      }
  getQuestion RequireGunLicense =
    Question
      { header = "Gun Licenses",
        info = "",
        source = "",
        questionTopic = "Guns",
        question = "All guns should require a license to own",
        qId = topic RequireGunLicense
      }

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
  getQuestion SinglePayerSystem =
    Question
      { header = "Single Payer Healthcare",
        info = "",
        source = "",
        questionTopic = "Healthcare",
        question = "The US should have a single-payer healthcare system",
        qId = topic SinglePayerSystem
      }
  getQuestion PublicHealthInsurance =
    Question
      { header = "Public Health Insurance",
        info = "",
        source = "",
        questionTopic = "Healthcare",
        question = "The US should have a public health insurance option for those who want it",
        qId = topic PublicHealthInsurance
      }
  getQuestion EliminatePrivateHealthInsurance =
    Question
      { header = "Private Health Insurance",
        info = "",
        source = "",
        questionTopic = "Healthcare",
        question = "Private health insurance should be eliminated",
        qId = topic EliminatePrivateHealthInsurance
      }
  getQuestion ImportPrescriptionDrugsFromCanada =
    Question
      { header = "Importing Prescription Drugs from Canada",
        info = "",
        source = "",
        questionTopic = "Healthcare",
        question = "The US should import some prescription drugs from Canada",
        qId = topic ImportPrescriptionDrugsFromCanada
      }

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
  getQuestion TrumpBorderWall =
    Question
      { header = "The Border Wall",
        info = "",
        source = "",
        questionTopic = "Immigration",
        question = "The border wall is a good idea",
        qId = topic TrumpBorderWall
      }
  getQuestion TrumpTravelBan =
    Question
      { header = "Trump's Travel Ban",
        info = "",
        source = "",
        questionTopic = "Immigration",
        question = "Trump's travel ban was a good idea",
        qId = topic TrumpTravelBan
      }
  getQuestion SupportDACA =
    Question
      { header = "DACA",
        info = "",
        source = "",
        questionTopic = "Immigration",
        question = "The US should support DACA",
        qId = topic SupportDACA
      }
  getQuestion AllowMoreVisaWorkers =
    Question
      { header = "Visa Workers",
        info = "",
        source = "",
        questionTopic = "Immigration",
        question = "The US should allow more visa workers in",
        qId = topic AllowMoreVisaWorkers
      }
  getQuestion DemilitarizeMexicoUSBorder =
    Question
      { header = "Demilitarizing the Mexico-US Border",
        info = "",
        source = "",
        questionTopic = "Immigration",
        question = "The Mexico-US border should be demilitarized",
        qId = topic DemilitarizeMexicoUSBorder
      }
  getQuestion InvestInPortsOfEntry =
    Question
      { header = "Ports of Entry",
        info = "",
        source = "",
        questionTopic = "Immigration",
        question = "The US should invest in ports of entry",
        qId = topic InvestInPortsOfEntry
      }
  getQuestion AbolishICE =
    Question
      { header = "ICE",
        info = "",
        source = "",
        questionTopic = "Immigration",
        question = "ICE should be abolished",
        qId = topic AbolishICE
      }
  getQuestion DecriminalizeIllegalImmigration =
    Question
      { header = "Illegal Immigration",
        info = "",
        source = "",
        questionTopic = "Immigration",
        question = "Illegal immigration shouldn't be a crime",
        qId = topic DecriminalizeIllegalImmigration
      }

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

questions :: [Question]
questions = getQuestion <$> topics
  where
    topics = getTopics @'[Education, Enviroment, Guns, Healthcare, Immigration]
