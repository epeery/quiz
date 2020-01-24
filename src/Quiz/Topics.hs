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
  ( Economics (..),
    Education (..),
    Enviroment (..),
    Guns (..),
    Healthcare (..),
    Immigration (..),
    Positions,
    Question,
    Technology (..),
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

type Topics =
  Education
    + Enviroment
    + Guns
    + Healthcare
    + Immigration
    + Technology
    + Economics

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
      { header = "Student Loan Debt",
        info = "Education debt in the U.S. has eclipsed credit card and auto debt. Today the average college graduate leaves school $30,000 in the red, up from $10,000 in the 1990s, and nearly 1 in 3 student loan borrowers are in delinquency or default. Research has shown that the debt makes it harder for people to buy houses and cars, start businesses and families, save or invest.",
        source = "https://www.cnbc.com/2019/09/21/what-the-2020-candidates-are-proposing-to-do-about-student-debt.html",
        questionTopic = "Education",
        question = "The government should offer debt relief for student loans",
        qId = topic DebtReliefForStudentLoans
      }
  getQuestion AffirmativeAction =
    Question
      { header = "Affirmative Action",
        info = "Affirmative action means positive steps taken to increase the representation of women and minorities in areas of employment, education, and culture from which they have been historically excluded. When those steps involve preferential selection—selection on the basis of race, gender, or ethnicity—affirmative action generates intense controversy.",
        source = "https://plato.stanford.edu/entries/affirmative-action/",
        questionTopic = "Education",
        question = "Affirmative action is a fundamentally good idea",
        qId = topic AffirmativeAction
      }
  getQuestion UniversalChildCare =
    Question
      { header = "Universal Child Care",
        info = "Ms. Warren’s plan, the Universal Child Care and Early Learning Act, would create a network of government-funded care centers based partly on the existing Head Start network, with employees paid comparably to public-school teachers. Families earning less than 200 percent of the federal poverty level would be able to send their children to these centers for free. Families earning more than that would be charged on a sliding scale, up to a maximum of 7 percent of their income.",
        source = "https://www.nytimes.com/2019/02/19/us/politics/elizabeth-warren-child-care.html",
        questionTopic = "Education",
        question = "I am in favor of universal child care",
        qId = topic UniversalChildCare
      }
  getQuestion UniversalPreKindergarten =
    Question
      { header = "Universal Pre-Kindergarten",
        info = "Universal Pre-K is a movement within the American education system to make access to preschool education available to all families, similar to the way kindergarten is available to all 5- and 6-year-olds. Like kindergarten, the pre-K idea is to provide voluntary education programs that include homeschooling and alternative education.",
        source = "https://www.rasmussen.edu/degrees/education/blog/universal-pre-k-what-is-it-why-affect-me/",
        questionTopic = "Education",
        question = "I am in favor of universal pre-kindergarten",
        qId = topic UniversalPreKindergarten
      }
  getQuestion IncreaseFundingForPublicEducation =
    Question
      { header = "Public Education",
        info = "State schools, called public schools in North America and many other countries, are generally primary or secondary schools mandated for or offered to all children without charge, funded in whole or in part by taxation.",
        source = "https://en.wikipedia.org/wiki/State_school",
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
        info = "The Green New Deal is a 10-year plan to mobilize every aspect of American society to 100% clean and renewable energy by 2030, a guaranteed living-wage job for anyone who needs one, and a just transition for both workers and frontline communities.",
        source = "https://www.sunrisemovement.org/green-new-deal",
        questionTopic = "Enviroment",
        question = "The Green New Deal is a great idea",
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
        info = "Nuclear power is a clean and efficient way of boiling water to make steam, which turns turbines to produce electricity. Nuclear power plants use low-enriched uranium fuel to produce electricity through a process called fission—the splitting of uranium atoms in a nuclear reactor. Uranium fuel consists of small, hard ceramic pellets that are packaged into long, vertical tubes. Bundles of this fuel are inserted into the reactor.",
        source = "https://nuclear.gepower.com/company-info/nuclear-power-basics",
        questionTopic = "Enviroment",
        question = "Leveraging nuclear power is a good way for the US to reduce emissions",
        qId = topic NuclearPowerToReduceEmissions
      }
  getQuestion CarbonTax =
    Question
      { header = "Carbon Tax",
        info = "A carbon tax is a fee that a government imposes on any company that burns fossil fuels. The most widely-discussed are coal, oil, gasoline, and natural gas. When these carbon-rich fuels are burned they produce greenhouse gases. These gases, such as carbon dioxide and methane, create global warming by heating the atmosphere. The resultant climate disruption causes extreme weather such as heat waves, flooding, blizzards, and droughts.",
        source = "https://www.thebalance.com/carbon-tax-definition-how-it-works-4158043",
        questionTopic = "Enviroment",
        question = "Any company that burns fossil fuels should have to pay a carbon tax",
        qId = topic CarbonTax
      }
  getQuestion ParisAgreement =
    Question
      { header = "The Paris Climate Agreement",
        info = "The Paris Agreement’s central aim is to strengthen the global response to the threat of climate change by keeping a global temperature rise this century well below 2 degrees Celsius above pre-industrial levels and to pursue efforts to limit the temperature increase even further to 1.5 degrees Celsius. Additionally, the agreement aims to increase the ability of countries to deal with the impacts of climate change, and at making finance flows consistent with a low GHG emissions and climate-resilient pathway. To reach these ambitious goals, appropriate mobilization and provision of financial resources, a new technology framework and enhanced capacity-building is to be put in place, thus supporting action by developing countries and the most vulnerable countries, in line with their own national objectives.",
        source = "https://unfccc.int/process-and-meetings/the-paris-agreement/what-is-the-paris-agreement",
        questionTopic = "Enviroment",
        question = "The US should rejoin the Paris Climate Agreement",
        qId = topic ParisAgreement
      }
  getQuestion BanFracking =
    Question
      { header = "Fracking",
        info = "Fracking is the process of drilling down into the earth before a high-pressure water mixture is directed at the rock to release the gas inside. Water, sand and chemicals are injected into the rock at high pressure which allows the gas to flow out to the head of the well. The process can be carried out vertically or, more commonly, by drilling horizontally to the rock layer, which can create new pathways to release gas or used to extend existing channels.",
        source = "https://www.bbc.com/news/uk-14432401",
        questionTopic = "Enviroment",
        question = "Fracking should be banned in the United States",
        qId = topic BanFracking
      }
  getQuestion BanOffshoreDrilling =
    Question
      { header = "Offshore Drilling",
        info = "Offshore drilling is the process of extracting petroleum from reserves located beneath the Earth's oceans instead of reserves located on the mainland. Offshore oil rigs have developed greatly over the past years, and have become gigantic structures that house hundreds of people at a time.",
        source = "https://energyeducation.ca/encyclopedia/Offshore_drilling",
        questionTopic = "Enviroment",
        question = "Offshore drilling should be banned",
        qId = topic BanOffshoreDrilling
      }
  getQuestion DeclareClimateChangeANationalEmergency =
    Question
      { header = "National Emergencies",
        info = "In 1976, Congress passed the National Emergencies Act, which permits the president to pronounce a national emergency when he considers it appropriate. The act offers no specific definition of “emergency” and allows a president to declare one entirely at his or her discretion. By declaring a national emergency, the president avails himself or herself of dozens of specialized laws. Some of these powers have funds the president otherwise could not access.",
        source = "https://www.washingtonpost.com/politics/2019/02/15/what-exactly-is-national-emergency-heres-what-that-means-what-happens-next/",
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
        info = "Proposals for universal background checks would require almost all firearms transactions in the United States to be recorded and go through the National Instant Criminal Background Check System (NICS), closing what is sometimes called the private sale exemption.",
        source = "https://en.wikipedia.org/wiki/Universal_background_check",
        questionTopic = "Guns",
        question = "The law should require anyone trying to buy a gun to have a background check done of them",
        qId = topic UniversalBackgroundChecks
      }
  getQuestion BanAssaultWeapons =
    Question
      { header = "Assault Weapons",
        info = "The gun industry’s traditional definition of an “assault rifle” is a weapon the military generally uses and has “select fire capabilities,” or the capability to switch between semi-automatic or a fully automatic mode. However, the civilian AR-15s do not have the select fire capabilities, only semi-automatic settings, so the firearms industry insists they are not an actual assault rifle or assault weapon.",
        source = "https://www.cnbc.com/2018/02/21/definition-of-whats-an-assault-weapon-is-a-very-contentious-issue.html",
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
      { header = "Deferred Action for Childhood Arrivals",
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
      { header = "Immigration and Customs Enforcement",
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

data Technology
  = ReinstateNetNeutrality
  | DataAsPersonalProperty
  | CASEAct
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance IsTopic Technology where
  getQuestion ReinstateNetNeutrality =
    Question
      { header = "Net Neutrality",
        info = "",
        source = "",
        questionTopic = "Technology",
        question = "Net neutrality should be reinstated",
        qId = topic ReinstateNetNeutrality
      }
  getQuestion DataAsPersonalProperty =
    Question
      { header = "Data as Property",
        info = "",
        source = "",
        questionTopic = "Technology",
        question = "Private data should be treated the same as personal property",
        qId = topic DataAsPersonalProperty
      }
  getQuestion CASEAct =
    Question
      { header = "The CASE Act",
        info = "",
        source = "",
        questionTopic = "Technology",
        question = "I am in favor of the CASE Act",
        qId = topic CASEAct
      }

instance Injectable Technology where
  inject = topic

instance ToJSON Technology

instance FromJSON Technology

data Economics
  = EstateTax
  | PostalBanking
  | ReparationsForSlavery
  | WealthTax
  | BreakingUpLargestBanks
  | SupportNAFTA
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance IsTopic Economics where
  getQuestion EstateTax =
    Question
      { header = "Estate Tax",
        info = "",
        source = "",
        questionTopic = "Economics",
        question = "I am in favor of a national estate tax",
        qId = topic EstateTax
      }
  getQuestion PostalBanking =
    Question
      { header = "Postal Banking",
        info = "",
        source = "",
        questionTopic = "Economics",
        question = "I am in favor of postal banking",
        qId = topic PostalBanking
      }
  getQuestion ReparationsForSlavery =
    Question
      { header = "Reparations for Slavery",
        info = "",
        source = "",
        questionTopic = "Economics",
        question = "American descendants of slaves should receive reparations for slavery",
        qId = topic ReparationsForSlavery
      }
  getQuestion WealthTax =
    Question
      { header = "Wealth Tax",
        info = "",
        source = "",
        questionTopic = "Economics",
        question = "The US should implement a wealth tax",
        qId = topic WealthTax
      }
  getQuestion BreakingUpLargestBanks =
    Question
      { header = "Breaking Up the Largest Banks",
        info = "",
        source = "",
        questionTopic = "Economics",
        question = "The government should break up the largest banks",
        qId = topic BreakingUpLargestBanks
      }
  getQuestion SupportNAFTA =
    Question
      { header = "The North American Free Trade Agreement",
        info = "",
        source = "",
        questionTopic = "Economics",
        question = "I support NAFTA",
        qId = topic SupportNAFTA
      }

instance Injectable Economics where
  inject = topic

instance ToJSON Economics

instance FromJSON Economics

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
-- topics = getTopics @'[Education, Enviroment, Guns, Healthcare, Immigration, Technology, Economics]
