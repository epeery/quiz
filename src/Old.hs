{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Old
  (
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

--
-- Topics are a grouping of questions
-- Positions are questions with opinions
--

data Topic
  = EducationT Education
  | EnviromentT Enviroment
  | GunsT Guns
  | HealthcareT Healthcare
  | ImmigrationT Immigration
  deriving (Show, Eq, Ord)

instance IsQuestion Topic where
  getQuestion (EducationT a) = getQuestion a
  getQuestion (EnviromentT a) = getQuestion a
  getQuestion (GunsT a) = getQuestion a
  getQuestion (HealthcareT a) = getQuestion a
  getQuestion (ImmigrationT a) = getQuestion a

class (Bounded a, Enum a) => IsTopic a where
  topic :: a -> Topic

class IsQuestion a where
  getQuestion :: a -> String

class IsTopicList a where
  getTopics :: [Topic]

instance IsTopicList '[] where
  getTopics = []

-- Example:
-- getTopics @'[Education, Healthcare, Immigration]
instance (IsTopic a, IsTopicList xs) => IsTopicList (a ': xs) where
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
  topic = EducationT

instance IsQuestion Education where
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

instance IsTopic Enviroment where
  topic = EnviromentT

instance IsQuestion Enviroment where
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

instance IsTopic Guns where
  topic = GunsT

instance IsQuestion Guns where
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

instance IsTopic Healthcare where
  topic = HealthcareT

instance IsQuestion Healthcare where
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

instance IsTopic Immigration where
  topic = ImmigrationT

instance IsQuestion Immigration where
  getQuestion TrumpBorderWall = "Trump's border wall is a good idea"
  getQuestion TrumpTravelBan = "Trump's travel ban was a good idea"
  getQuestion SupportDACA = "The US should support DACA"
  getQuestion AllowMoreVisaWorkers = "The US should allow more visa workers in"
  getQuestion DemilitarizeMexicoUSBorder = "The Mexico-US border should be demilitarized"
  getQuestion InvestInPortsOfEntry = "The US should invest in ports of entry"
  getQuestion AbolishICE = "ICE should be abolished"
  getQuestion DecriminalizeIllegalImmigration = "Illegal immigration shouldn't be a crime"

type Positions = Map Topic Float

lookupTopic :: Topic -> Positions -> Float
lookupTopic = M.findWithDefault 0

-- Temporary way to compare positions
-- Lower number == more similar
comparePositions :: Positions -> Positions -> Float
comparePositions p1 p2 = foldr f 0 (M.toList p1)
  where
    f (p, x) acc = (acc +) . abs $ x - (lookupTopic p p2)
