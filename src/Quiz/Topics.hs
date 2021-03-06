{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Quiz.Topics
  ( Economics (..),
    Education (..),
    Environment (..),
    Guns (..),
    Healthcare (..),
    Immigration (..),
    LaborAndWelfare (..),
    Positions,
    Question,
    Technology (..),
    Topics,
    comparePositions,
    getQuestion,
    parseTopics,
    percentageMatch,
    questions,
    readQuestion,
    topic,
  )
where

import Control.Applicative
import Data.Aeson.Types
import Data.List (filter)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack)
import Data.Typeable (Proxy (..), Typeable, typeRep)
import Data.Void
import Deriving.Aeson
import Text.Read (readMaybe)

type Topics = Summed TopicList

type TopicList =
  '[ Education,
     Environment,
     Guns,
     Healthcare,
     Immigration,
     Technology,
     Economics,
     LaborAndWelfare
   ]

class Summable (fs :: [*]) where
  data Summed fs

instance Summable '[] where
  data Summed '[] = SummedNil Void

instance Summable (a ': as) where
  data Summed (a ': as)
    = InL a
    | InR (Summed as)

class Injectable (a :: *) (as :: [*]) where
  inj :: a -> Summed as

instance Injectable a (a ': as) where
  inj = InL

instance {-# OVERLAPPABLE #-} Injectable a as => Injectable a (b ': as) where
  inj = InR . inj

instance Show (Summed '[]) where
  show _ = "[]"

instance (Show a, Show (Summed as)) => Show (Summed (a ': as)) where
  show (InL a) = "InL " ++ show a
  show (InR a) = "InR " ++ show a

instance Eq (Summed '[]) where
  (==) _ _ = True

instance Ord (Summed '[]) where
  compare _ _ = EQ

instance (Eq a, Eq (Summed as)) => Eq (Summed (a ': as)) where
  (==) (InL a) (InL b) = a == b
  (==) (InL _) _ = False
  (==) (InR a) (InR b) = a == b
  (==) (InR _) _ = False

instance (Ord a, Ord (Summed as)) => Ord (Summed (a ': as)) where
  compare (InL a) (InL b) = compare a b
  compare (InL _) _ = LT
  compare (InR a) (InR b) = compare a b
  compare (InR _) _ = GT

instance ToJSON (Summed '[])

deriving instance Generic (Summed '[])

instance (ToJSON f, ToJSON (Summed fs)) => ToJSON (Summed (f ': fs))

instance FromJSON (Summed '[])

instance (FromJSON f, FromJSON (Summed fs)) => FromJSON (Summed (f ': fs))

deriving instance Generic (Summed (f ': fs))

instance (ToJSON a, ToJSON (Summed as)) => ToJSONKey (Summed (a ': as))

instance (FromJSON a, FromJSON (Summed as)) => FromJSONKey (Summed (a ': as))

class
  ( Summable fs,
    Injectable f fs
  ) =>
  f :<: (fs :: [*])

instance
  ( Summable fs,
    Injectable f fs
  ) =>
  (f :<: fs)

class IsTopicList (a :: [*]) where
  getTopics :: [Topics]

instance IsTopicList '[] where
  getTopics = []

-- Given a type level list of topics, returns a list of questions
-- Example:
-- getTopics @'[Education, Environment, Healthcare]
instance (a :<: TopicList, Bounded a, Enum a, IsTopicList xs) => IsTopicList (a ': xs) where
  getTopics = (inj @a <$> [minBound .. maxBound]) ++ getTopics @xs

class Readable a b where
  checkRead :: String -> Maybe b

instance Readable '[] a where
  checkRead _ = Nothing

instance (Read a, a :<: TopicList, Readable as Topics) => Readable (a ': as) Topics where
  checkRead s = (readMaybe @a s >>= return . inj) <|> (checkRead @as @Topics s)

-- Given a question name, returns the encoded version
readQuestion :: String -> Maybe Topics
readQuestion = checkRead @TopicList

class Parseable a where
  parseTopic :: String -> [Topics]

instance Parseable '[] where
  parseTopic _ = []

instance (Typeable a, IsTopicList '[a], Parseable as) => Parseable (a ': as) where
  parseTopic s = if s == (show $ typeRep (Proxy @a)) then getTopics @'[a] else parseTopic @as s

-- Given a list of topic names, returns a list of questions
parseTopics :: [String] -> [Topics]
parseTopics = foldMap (parseTopic @TopicList)

class IsTopic a where
  getQuestion :: a -> Question

instance IsTopic (Summed '[]) where
  getQuestion _ = undefined

instance (IsTopic a, IsTopic (Summed b)) => IsTopic (Summed (a ': b)) where
  getQuestion (InL x) = getQuestion x
  getQuestion (InR y) = getQuestion y

-- Renamed for more obvious API
topic :: (a :<: TopicList) => a -> Topics
topic = inj

data Question
  = Question
      { questionQuestion :: Text,
        questionHeader :: Text,
        questionInfo :: Text,
        questionSource :: Text,
        questionTopic :: Text,
        questionId :: Text
      }
  deriving (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "question", CamelToSnake)] Question

type Positions = Map Topics Double

lookupTopic :: Topics -> Positions -> Double
lookupTopic = M.findWithDefault 0

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
questions = getQuestion <$> getTopics @TopicList

------------------------------------------------------------------------
--                         Topic definitions                          --
------------------------------------------------------------------------

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
      { questionHeader = "Tuition Free Public College",
        questionInfo = "Most Democrats have gotten behind the idea of some form of tuition-free or debt-free college, but they disagree about how much of the tab should be covered. Several candidates have called for making four years of public college free for students under a certain income threshold while others would cover only community college or technical school.",
        questionSource = "https://www.politico.com/2020-election/candidates-views-on-the-issues/education-reform/free-college/",
        questionTopic = "Education",
        questionQuestion = "Public college should be tuition free",
        questionId = pack . show $ TuitionFreePublicCollege
      }
  getQuestion DebtReliefForStudentLoans =
    Question
      { questionHeader = "Student Loan Debt",
        questionInfo = "Education debt in the U.S. has eclipsed credit card and auto debt. Today the average college graduate leaves school $30,000 in the red, up from $10,000 in the 1990s, and nearly 1 in 3 student loan borrowers are in delinquency or default. Research has shown that the debt makes it harder for people to buy houses and cars, start businesses and families, save or invest.",
        questionSource = "https://www.cnbc.com/2019/09/21/what-the-2020-candidates-are-proposing-to-do-about-student-debt.html",
        questionTopic = "Education",
        questionQuestion = "The government should offer debt relief for student loans",
        questionId = pack . show $ DebtReliefForStudentLoans
      }
  getQuestion AffirmativeAction =
    Question
      { questionHeader = "Affirmative Action",
        questionInfo = "Affirmative action means positive steps taken to increase the representation of women and minorities in areas of employment, education, and culture from which they have been historically excluded. When those steps involve preferential selection—selection on the basis of race, gender, or ethnicity—affirmative action generates intense controversy.",
        questionSource = "https://plato.stanford.edu/entries/affirmative-action/",
        questionTopic = "Education",
        questionQuestion = "Affirmative action is a fundamentally good idea",
        questionId = pack . show $ AffirmativeAction
      }
  getQuestion UniversalChildCare =
    Question
      { questionHeader = "Universal Child Care",
        questionInfo = "Ms. Warren’s plan, the Universal Child Care and Early Learning Act, would create a network of government-funded care centers based partly on the existing Head Start network, with employees paid comparably to public-school teachers. Families earning less than 200 percent of the federal poverty level would be able to send their children to these centers for free. Families earning more than that would be charged on a sliding scale, up to a maximum of 7 percent of their income.",
        questionSource = "https://www.nytimes.com/2019/02/19/us/politics/elizabeth-warren-child-care.html",
        questionTopic = "Education",
        questionQuestion = "I am in favor of universal child care",
        questionId = pack . show $ UniversalChildCare
      }
  getQuestion UniversalPreKindergarten =
    Question
      { questionHeader = "Universal Pre-Kindergarten",
        questionInfo = "Universal Pre-K is a movement within the American education system to make access to preschool education available to all families, similar to the way kindergarten is available to all 5- and 6-year-olds. Like kindergarten, the pre-K idea is to provide voluntary education programs that include homeschooling and alternative education.",
        questionSource = "https://www.rasmussen.edu/degrees/education/blog/universal-pre-k-what-is-it-why-affect-me/",
        questionTopic = "Education",
        questionQuestion = "I am in favor of universal pre-kindergarten",
        questionId = pack . show $ UniversalPreKindergarten
      }
  getQuestion IncreaseFundingForPublicEducation =
    Question
      { questionHeader = "Public Education",
        questionInfo = "State schools, called public schools in North America and many other countries, are generally primary or secondary schools mandated for or offered to all children without charge, funded in whole or in part by taxation.",
        questionSource = "https://en.wikipedia.org/wiki/State_school",
        questionTopic = "Education",
        questionQuestion = "The government should increase funding for primary and secondary public education",
        questionId = pack . show $ IncreaseFundingForPublicEducation
      }

instance ToJSON Education

instance FromJSON Education

data Environment
  = GreenNewDeal
  | NoFossilFuelMoneyPledge
  | NuclearPowerToReduceEmissions
  | CarbonTax
  | ParisAgreement
  | BanFracking
  | BanOffshoreDrilling
  | DeclareClimateChangeANationalEmergency
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance IsTopic Environment where
  getQuestion GreenNewDeal =
    Question
      { questionHeader = "The Green New Deal",
        questionInfo = "The Green New Deal is a 10-year plan to mobilize every aspect of American society to 100% clean and renewable energy by 2030, a guaranteed living-wage job for anyone who needs one, and a just transition for both workers and frontline communities.",
        questionSource = "https://www.sunrisemovement.org/green-new-deal",
        questionTopic = "Environment",
        questionQuestion = "The Green New Deal is a good idea",
        questionId = pack . show $ GreenNewDeal
      }
  getQuestion NoFossilFuelMoneyPledge =
    Question
      { questionHeader = "The No Fossil Fuel Money Pledge",
        questionInfo = "I pledge not to take contributions over $200 from oil, gas, and coal industry executives, lobbyists, and PACs and instead prioritize the health of our families, climate, and democracy over fossil fuel industry profits.",
        questionSource = "http://nofossilfuelmoney.org/",
        questionTopic = "Environment",
        questionQuestion = "It is important to me that my candidate has taken the No Fossil Fuel Money Pledge",
        questionId = pack . show $ NoFossilFuelMoneyPledge
      }
  getQuestion NuclearPowerToReduceEmissions =
    Question
      { questionHeader = "Nuclear Power To Reduce Emissions",
        questionInfo = "Nuclear power is a clean and efficient way of boiling water to make steam, which turns turbines to produce electricity. Nuclear power plants use low-enriched uranium fuel to produce electricity through a process called fission—the splitting of uranium atoms in a nuclear reactor. Uranium fuel consists of small, hard ceramic pellets that are packaged into long, vertical tubes. Bundles of this fuel are inserted into the reactor.",
        questionSource = "https://nuclear.gepower.com/company-info/nuclear-power-basics",
        questionTopic = "Environment",
        questionQuestion = "Leveraging nuclear power is a good way for the U.S. to reduce emissions",
        questionId = pack . show $ NuclearPowerToReduceEmissions
      }
  getQuestion CarbonTax =
    Question
      { questionHeader = "Carbon Tax",
        questionInfo = "A carbon tax is a fee that a government imposes on any company that burns fossil fuels. The most widely-discussed are coal, oil, gasoline, and natural gas. When these carbon-rich fuels are burned they produce greenhouse gases. These gases, such as carbon dioxide and methane, create global warming by heating the atmosphere. The resultant climate disruption causes extreme weather such as heat waves, flooding, blizzards, and droughts.",
        questionSource = "https://www.thebalance.com/carbon-tax-definition-how-it-works-4158043",
        questionTopic = "Environment",
        questionQuestion = "Any company that burns fossil fuels should have to pay a carbon tax",
        questionId = pack . show $ CarbonTax
      }
  getQuestion ParisAgreement =
    Question
      { questionHeader = "The Paris Climate Agreement",
        questionInfo = "The Paris Agreement’s central aim is to strengthen the global response to the threat of climate change by keeping a global temperature rise this century well below 2 degrees Celsius above pre-industrial levels and to pursue efforts to limit the temperature increase even further to 1.5 degrees Celsius. Additionally, the agreement aims to increase the ability of countries to deal with the impacts of climate change, and at making finance flows consistent with a low GHG emissions and climate-resilient pathway. To reach these ambitious goals, appropriate mobilization and provision of financial resources, a new technology framework and enhanced capacity-building is to be put in place, thus supporting action by developing countries and the most vulnerable countries, in line with their own national objectives.",
        questionSource = "https://unfccc.int/process-and-meetings/the-paris-agreement/what-is-the-paris-agreement",
        questionTopic = "Environment",
        questionQuestion = "The U.S. should rejoin the Paris Climate Agreement",
        questionId = pack . show $ ParisAgreement
      }
  getQuestion BanFracking =
    Question
      { questionHeader = "Fracking",
        questionInfo = "Fracking is the process of drilling down into the earth before a high-pressure water mixture is directed at the rock to release the gas inside. Water, sand and chemicals are injected into the rock at high pressure which allows the gas to flow out to the head of the well. The process can be carried out vertically or, more commonly, by drilling horizontally to the rock layer, which can create new pathways to release gas or used to extend existing channels.",
        questionSource = "https://www.bbc.com/news/uk-14432401",
        questionTopic = "Environment",
        questionQuestion = "Fracking should be banned in the United States",
        questionId = pack . show $ BanFracking
      }
  getQuestion BanOffshoreDrilling =
    Question
      { questionHeader = "Offshore Drilling",
        questionInfo = "Offshore drilling is the process of extracting petroleum from reserves located beneath the Earth's oceans instead of reserves located on the mainland. Offshore oil rigs have developed greatly over the past years, and have become gigantic structures that house hundreds of people at a time.",
        questionSource = "https://energyeducation.ca/encyclopedia/Offshore_drilling",
        questionTopic = "Environment",
        questionQuestion = "Offshore drilling should be banned",
        questionId = pack . show $ BanOffshoreDrilling
      }
  getQuestion DeclareClimateChangeANationalEmergency =
    Question
      { questionHeader = "National Emergencies",
        questionInfo = "In 1976, Congress passed the National Emergencies Act, which permits the president to pronounce a national emergency when he considers it appropriate. The act offers no specific definition of “emergency” and allows a president to declare one entirely at his or her discretion. By declaring a national emergency, the president avails himself or herself of dozens of specialized laws. Some of these powers have funds the president otherwise could not access.",
        questionSource = "https://www.washingtonpost.com/politics/2019/02/15/what-exactly-is-national-emergency-heres-what-that-means-what-happens-next/",
        questionTopic = "Environment",
        questionQuestion = "Climate change should be declared a national emergency",
        questionId = pack . show $ DeclareClimateChangeANationalEmergency
      }

instance ToJSON Environment

instance FromJSON Environment

data Guns
  = UniversalBackgroundChecks
  | BanAssaultWeapons
  | GunBuyBack
  | RequireGunLicense
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance IsTopic Guns where
  getQuestion UniversalBackgroundChecks =
    Question
      { questionHeader = "Universal Background Checks",
        questionInfo = "Proposals for universal background checks would require almost all firearms transactions in the United States to be recorded and go through the National Instant Criminal Background Check System (NICS), closing what is sometimes called the private sale exemption.",
        questionSource = "https://en.wikipedia.org/wiki/Universal_background_check",
        questionTopic = "Guns",
        questionQuestion = "The law should require anyone trying to buy a gun to have a background check done on them",
        questionId = pack . show $ UniversalBackgroundChecks
      }
  getQuestion BanAssaultWeapons =
    Question
      { questionHeader = "Assault Weapons",
        questionInfo = "The gun industry’s traditional definition of an “assault rifle” is a weapon the military generally uses and has “select fire capabilities,” or the capability to switch between semi-automatic or a fully automatic mode. However, the civilian AR-15s do not have the select fire capabilities, only semi-automatic settings, so the firearms industry insists they are not an actual assault rifle or assault weapon.",
        questionSource = "https://www.cnbc.com/2018/02/21/definition-of-whats-an-assault-weapon-is-a-very-contentious-issue.html",
        questionTopic = "Guns",
        questionQuestion = "Assault weapons should be banned",
        questionId = pack . show $ BanAssaultWeapons
      }
  getQuestion GunBuyBack =
    Question
      { questionHeader = "Gun Buyback Program",
        questionInfo = "Gun buybacks are events where individuals can turn in firearms to law enforcement, usually with no questions asked, and receive some kind of compensation in return. The overall goal of gun buyback programs is to reduce the number of gun deaths and injuries in a community.",
        questionSource = "https://www.gunxgun.org/buybacks_faq",
        questionTopic = "Guns",
        questionQuestion = "The government should implement a gun buyback program",
        questionId = pack . show $ GunBuyBack
      }
  getQuestion RequireGunLicense =
    Question
      { questionHeader = "Gun Licensing Laws",
        questionInfo = "Licensing laws ensure that gun owners have passed a background check before they purchase a gun. In contrast to states which require a background check at the point of sale of a firearm, licensing laws typically require an in-person application at law enforcement agencies, which provides an additional safeguard against fraud or inaccuracies that could allow dangerous individuals to obtain guns.",
        questionSource = "https://lawcenter.giffords.org/gun-laws/policy-areas/gun-owner-responsibilities/licensing/",
        questionTopic = "Guns",
        questionQuestion = "All guns should require a license to own",
        questionId = pack . show $ RequireGunLicense
      }

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
      { questionHeader = "Single Payer Healthcare",
        questionInfo = "In a single payer healthcare system, rather than multiple competing health insurance companies, a single public or quasi-public agency takes responsibility for financing healthcare for all residents. That is, everyone has health insurance under a one health insurance plan, and has access to necessary services — including doctors, hospitals, long-term care, prescription drugs, dentists and vision care. However, individuals may still choose where they receive care. It’s a lot like Medicare, hence the U.S. single payer nickname “Medicare-for-all.",
        questionSource = "https://www.health.harvard.edu/blog/single-payer-healthcare-pluses-minuses-means-201606279835",
        questionTopic = "Healthcare",
        questionQuestion = "The U.S. should have a single-payer healthcare system",
        questionId = pack . show $ SinglePayerSystem
      }
  getQuestion PublicHealthInsurance =
    Question
      { questionHeader = "Public Health Insurance",
        questionInfo = "A program run by U.S. federal, state, or local governments in which people have some or all of their healthcare costs paid for by the government. The two main types of public health insurance are Medicare and Medicaid. Medicare is a federal health insurance program for people aged 65 years or older and people with certain disabilities. Medicaid is a public health insurance program for some individuals and families with a low income or disabilities.",
        questionSource = "https://www.cancer.gov/publications/dictionaries/cancer-terms/def/public-health-insurance",
        questionTopic = "Healthcare",
        questionQuestion = "The U.S. should have some form of public health insurance",
        questionId = pack . show $ PublicHealthInsurance
      }
  getQuestion EliminatePrivateHealthInsurance =
    Question
      { questionHeader = "Private Health Insurance",
        questionInfo = "Private health insurance refers to health insurance plans marketed by the private health insurance industry, as opposed to government-run insurance programs. Private health insurance currently dominates the U.S. health care landscape, covering more than half of the U.S. population.",
        questionSource = "https://www.healthinsurance.org/glossary/private-health-insurance/",
        questionTopic = "Healthcare",
        questionQuestion = "Private health insurance should be eliminated",
        questionId = pack . show $ EliminatePrivateHealthInsurance
      }
  getQuestion ImportPrescriptionDrugsFromCanada =
    Question
      { questionHeader = "Prescription Drugs in Canada",
        questionInfo = "Canadian drugs are cheaper because the government regulates the price of generic and brand-name medication. The prices of cheaper generic drugs, which account for most prescriptions, are set through deals with drug companies at the provincial and national levels.",
        questionSource = "https://www.cbc.ca/news/politics/us-drug-plan-canadian-shortage-1.5232360",
        questionTopic = "Healthcare",
        questionQuestion = "The U.S. should import some prescription drugs from Canada",
        questionId = pack . show $ ImportPrescriptionDrugsFromCanada
      }

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
      { questionHeader = "Potential Effectiveness of a Border Wall",
        questionInfo = "Many contemporary examples of border barriers are considered to be effective. Barriers such as the Hungarian border barrier, the Israeli border walls, and the Israeli West Bank barrier have lowered the number of illegal border crossings. In Hungary, for example, the number of illegal immigrants dropped from 4500 per day to 15 after a 175-kilometer long, four-meter high fence was constructed in 2015. On the other hand, research at Texas A&M University and Texas Tech University indicates that the wall, and border walls in general, are unlikely to be effective at reducing illegal immigration or movement of contraband.",
        questionSource = "https://en.wikipedia.org/wiki/Trump_wall#Effectiveness",
        questionTopic = "Immigration",
        questionQuestion = "The border wall is a good idea",
        questionId = pack . show $ TrumpBorderWall
      }
  getQuestion TrumpTravelBan =
    Question
      { questionHeader = "Trump's Travel Ban",
        questionInfo = "The Trump travel ban (sometimes called the \"Muslim ban\") denotes a series of executive actions enacted by Donald Trump as President of the United States in 2017. First, Executive Order 13769 placed stringent restrictions on travel to the United States for citizens of Iran, Iraq, Libya, Somalia, Sudan, Syria, and Yemen. Following protests and legal challenges, a second order, Executive Order 13780, amended some provisions of the first order, and removed Iraq from the list. Finally, Presidential Proclamation 9645 added restrictions on Chad, North Korea, and Venezuela, while Sudan was removed. Six of the eight affected countries are predominantly Muslim.",
        questionSource = "https://en.wikipedia.org/wiki/Trump_travel_ban",
        questionTopic = "Immigration",
        questionQuestion = "Trump's travel ban was a good idea",
        questionId = pack . show $ TrumpTravelBan
      }
  getQuestion SupportDACA =
    Question
      { questionHeader = "Deferred Action for Childhood Arrivals",
        questionInfo = "Deferred Action for Childhood Arrivals (DACA) is a kind of administrative relief from deportation. The purpose of DACA is to protect eligible immigrant youth who came to the United States when they were children from deportation. DACA gives young undocumented immigrants: 1) protection from deportation, and 2) a work permit. The program expires after two years, subject to renewal.",
        questionSource = "https://undocu.berkeley.edu/legal-support-overview/what-is-daca/",
        questionTopic = "Immigration",
        questionQuestion = "The U.S. should support DACA",
        questionId = pack . show $ SupportDACA
      }
  getQuestion AllowMoreVisaWorkers =
    Question
      { questionHeader = "Work Visas",
        questionInfo = "The U.S.A remains one of the most popular immigration destination countries in the world. Employers can apply to employ skilled migrants under various non-immigrant work visa schemes.",
        questionSource = "https://workpermit.com/immigration/usa/us-immigration",
        questionTopic = "Immigration",
        questionQuestion = "The U.S. should allow more visa workers in",
        questionId = pack . show $ AllowMoreVisaWorkers
      }
  getQuestion DemilitarizeMexicoUSBorder =
    Question
      { questionHeader = "Militarization of the U.S.-Mexico Border",
        questionInfo = "The U.S.-Mexico border stands to become one of the world’s most militarized borders, even though the two countries are not at war. Massive increases in appropriations for border security have served only to decrease real security in the U.S., disrupting the quality of life and economies of border communities and eroding human rights in the region.",
        questionSource = "https://www.afsc.org/key-issues/issue/us-mexico-border-militarization",
        questionTopic = "Immigration",
        questionQuestion = "The U.S.-Mexico border should be demilitarized",
        questionId = pack . show $ DemilitarizeMexicoUSBorder
      }
  getQuestion InvestInPortsOfEntry =
    Question
      { questionHeader = "Ports of Entry",
        questionInfo = "In general, a port of entry (POE) is a place where one may lawfully enter a country. It typically has border security staff and facilities to check passports and visas, and inspect luggage to assure that contraband is not imported.",
        questionSource = "https://en.wikipedia.org/wiki/Port_of_entry",
        questionTopic = "Immigration",
        questionQuestion = "The U.S. should invest in ports of entry",
        questionId = pack . show $ InvestInPortsOfEntry
      }
  getQuestion AbolishICE =
    Question
      { questionHeader = "U.S. Immigration and Customs Enforcement",
        questionInfo = "The U.S. Immigration and Customs Enforcement (ICE) is a federal law enforcement agency under the U.S. Department of Homeland Security, principally responsible for immigration and customs enforcement, with additional responsibilities in countering transnational crime.",
        questionSource = "https://en.wikipedia.org/wiki/U.S._Immigration_and_Customs_Enforcement",
        questionTopic = "Immigration",
        questionQuestion = "ICE should be abolished",
        questionId = pack . show $ AbolishICE
      }
  getQuestion DecriminalizeIllegalImmigration =
    Question
      { questionHeader = "Illegal Immigration",
        questionInfo = "Illegal immigration refers to the migration of people into a country in violation of the immigration laws of that country, or the continued residence of people without the legal right to live in that country. Illegal immigration tends to be financially upward, from poorer to richer countries.",
        questionSource = "https://en.wikipedia.org/wiki/Illegal_immigration",
        questionTopic = "Immigration",
        questionQuestion = "Illegal immigration shouldn't be a crime",
        questionId = pack . show $ DecriminalizeIllegalImmigration
      }

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
      { questionHeader = "Net Neutrality",
        questionInfo = "Net neutrality is the idea that internet service providers like Comcast and Verizon should treat all content flowing through their cables and cell towers equally. That means they shouldn't be able to slide some data into “fast lanes” while blocking or otherwise discriminating against other material. In other words, these companies shouldn't be able to block you from accessing a service like Skype, or slow down Netflix or Hulu, in order to encourage you to keep your cable package or buy a different video-streaming service.",
        questionSource = "https://www.wired.com/story/guide-net-neutrality/",
        questionTopic = "Technology",
        questionQuestion = "Net neutrality should be reinstated",
        questionId = pack . show $ ReinstateNetNeutrality
      }
  getQuestion DataAsPersonalProperty =
    Question
      { questionHeader = "Data as Property",
        questionInfo = "During one of the recent Federal Trade Commission (FTC) workshops on competition policy, economist Dennis Carlton and others suggested that defining who has property rights in data is a key to properly analyzing data issues in antitrust and that a natural place to start would be that the individual owns the data including health care information and maybe search data as well. ",
        questionSource = "https://www.forbes.com/sites/washingtonbytes/2018/11/02/privacy-is-not-a-property-right-in-personal-information/#67e580dd280f",
        questionTopic = "Technology",
        questionQuestion = "Private data should be treated the same as personal property",
        questionId = pack . show $ DataAsPersonalProperty
      }
  getQuestion CASEAct =
    Question
      { questionHeader = "The Copyright Alternative in Small-Claims Enforcement Act",
        questionInfo = "The Copyright Alternative in Small-Claims Enforcement Act of 2019 (the CASE Act) is a proposed United States law that would provide a new means for copyright owners to file infringement claims; for copyright users to adjudicate declarations of non-infringement; for owners and users to submit claims related to Section 512(f) of the Digital Millennium Copyright Act; and for would-be defendants to submit counterclaims and legal defenses, such as fair use.",
        questionSource = "https://en.wikipedia.org/wiki/CASE_Act",
        questionTopic = "Technology",
        questionQuestion = "I am in favor of the CASE Act",
        questionId = pack . show $ CASEAct
      }

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
      { questionHeader = "The Estate Tax",
        questionInfo = "The Estate Tax is a tax on your right to transfer property at your death. It consists of an accounting of everything you own or have certain interests in at the date of death. The fair market value of these items is used, not necessarily what you paid for them or what their values were when you acquired them.",
        questionSource = "https://www.irs.gov/businesses/small-businesses-self-employed/estate-tax",
        questionTopic = "Economics",
        questionQuestion = "I am in favor of a national estate tax",
        questionId = pack . show $ EstateTax
      }
  getQuestion PostalBanking =
    Question
      { questionHeader = "Postal Banking",
        questionInfo = "...Requiring U.S. postal offices to offer basic financial services to customers, including checking accounts, interest-bearing savings accounts and short-term loans.",
        questionSource = "https://twocents.lifehacker.com/what-is-postal-banking-1828194701",
        questionTopic = "Economics",
        questionQuestion = "I am in favor of postal banking",
        questionId = pack . show $ PostalBanking
      }
  getQuestion ReparationsForSlavery =
    Question
      { questionHeader = "Reparations for Slavery",
        questionInfo = "Reparations for slavery is a political justice concept that argues that reparations should be paid to the descendants of slaves from Sub-Saharan African who were trafficked to and enslaved in the Americas as a consequence of the Atlantic slave trade.",
        questionSource = "https://en.wikipedia.org/wiki/Reparations_for_slavery",
        questionTopic = "Economics",
        questionQuestion = "American descendants of slaves should receive reparations for slavery",
        questionId = pack . show $ ReparationsForSlavery
      }
  getQuestion WealthTax =
    Question
      { questionHeader = "Wealth Tax",
        questionInfo = "It's an annual tax on the net wealth a person holds — so, their assets minus their debts. Not just the income they bring in each year. On the one hand, you can think of it as something like the property taxes people pay on their homes, but applied to all their wealth above a certain level. This is, in fact, a major way that Elizabeth Warren sells her wealth tax in her stump speech. But then, on the other hand, it would be a new type of tax for the federal government to levy — property taxes are usually imposed at the local level, and estate taxes, while on wealth, are only imposed at death.",
        questionSource = "https://www.npr.org/2019/12/05/782135614/how-would-a-wealth-tax-work#what",
        questionTopic = "Economics",
        questionQuestion = "The U.S. should implement a wealth tax",
        questionId = pack . show $ WealthTax
      }
  getQuestion BreakingUpLargestBanks =
    Question
      { questionHeader = "Breaking Up the Largest Banks",
        questionInfo = "The most basic reason to break up the banks is to increase the stability and security of our financial system. Average American consumers and business owners keep their deposits in commercial banks—but in addition to holding deposits, those banks also have divisions that invest in global markets. That exposes banks (and the deposits they hold) to risks in the broader economy.",
        questionSource = "https://www.fundera.com/blog/breaking-up-the-big-banks",
        questionTopic = "Economics",
        questionQuestion = "The government should break up the largest banks",
        questionId = pack . show $ BreakingUpLargestBanks
      }
  getQuestion SupportNAFTA =
    Question
      { questionHeader = "The North American Free Trade Agreement",
        questionInfo = "NAFTA's pros and cons are hotly debated. Critics point to three main disadvantages of NAFTA. First, it sent many U.S. manufacturing jobs to lower-cost Mexico. Second, workers who kept jobs in those industries had to accept lower wages. Third, Mexico's workers suffered exploitation in its maquiladora programs. But NAFTA also has three significant advantages. U.S. grocery prices would be higher without tariff-free imports from Mexico. Imported oil from both Canada and Mexico has prevented higher gas prices. NAFTA has also increased trade and economic growth for all three countries.",
        questionSource = "https://www.thebalance.com/nafta-definition-north-american-free-trade-agreement-3306147",
        questionTopic = "Economics",
        questionQuestion = "I am in support of NAFTA",
        questionId = pack . show $ SupportNAFTA
      }

instance ToJSON Economics

instance FromJSON Economics

data LaborAndWelfare
  = RaiseMinimumWage
  | BasicIncome
  | PaidFamilyLeave
  | PaidSickLeave
  | LimitRightToWorkLaws
  | JobGuarantee
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance IsTopic LaborAndWelfare where
  getQuestion RaiseMinimumWage =
    Question
      { questionHeader = "Minimum Wage",
        questionInfo = "The minimum wage in the United States is set by US labor law and a range of state and local laws. Employers generally have to pay workers the highest minimum wage prescribed by federal, state, and local law. Since July 24, 2009, the federal government has mandated a nationwide minimum wage of $7.25 per hour. As of January 2018, there were 29 states with a minimum wage higher than the federal minimum.",
        questionSource = "https://en.wikipedia.org/wiki/Minimum_wage_in_the_United_States",
        questionTopic = "Labor and Welfare",
        questionQuestion = "The federal minimum wage should be raised",
        questionId = pack . show $ RaiseMinimumWage
      }
  getQuestion BasicIncome =
    Question
      { questionHeader = "Universal Basic Income",
        questionInfo = "Basic income, also called universal basic income (UBI), ...is a governmental public program for a periodic payment delivered to all on an individual basis without means test or work requirement",
        questionSource = "https://en.wikipedia.org/wiki/Basic_income",
        questionTopic = "Labor and Welfare",
        questionQuestion = "U.S. citizens should be guaranteed at least a base level of income",
        questionId = pack . show $ BasicIncome
      }
  getQuestion PaidFamilyLeave =
    Question
      { questionHeader = "Paid Family Leave",
        questionInfo = "Paid family leave (PFL) refers to partially or fully compensated time away from work for specific and generally significant family caregiving needs, such as the arrival of a new child or serious illness of a close family member. Although the Family and Medical Leave Act of 1993 (FMLA;P.L. 103-3) provides eligible workers with a federal entitlement to unpaid leave for a limited set of family caregiving needs, no federal law requires private-sector employers to provide paid leave of any kind.",
        questionSource = "https://fas.org/sgp/crs/misc/R44835.pdf",
        questionTopic = "Labor and Welfare",
        questionQuestion = "Companies should be required to give employees paid family leave",
        questionId = pack . show $ PaidFamilyLeave
      }
  getQuestion PaidSickLeave =
    Question
      { questionHeader = "Paid Sick Leave",
        questionInfo = "Currently, there are no federal legal requirements for paid sick leave. For companies subject to the Family and Medical Leave Act (FMLA), the Act does require unpaid sick leave. FMLA provides for up to 12 weeks of unpaid leave for certain medical situations for either the employee or a member of the employee's immediate family. In many instances paid leave may be substituted for unpaid FMLA leave.",
        questionSource = "https://www.dol.gov/general/topic/workhours/sickleave",
        questionTopic = "Labor and Welfare",
        questionQuestion = "Companies should be required to give employees paid sick leave",
        questionId = pack . show $ PaidSickLeave
      }
  getQuestion LimitRightToWorkLaws =
    Question
      { questionHeader = "Right to Work Laws",
        questionInfo = "Right to work laws address situations related to memberships in labor unions -- In short, you never need to join a union or pay union dues to be hired or to work for a company. Commonly, these laws involve employers refusing to hire non-union workers or requiring that workers join a union as a condition of employment. This behavior is illegal because people have the \"right to work\" without being part of a union under state and federal laws.",
        questionSource = "https://employment.findlaw.com/wages-and-benefits/what-are-right-to-work-laws.html",
        questionTopic = "Labor and Welfare",
        questionQuestion = "Right to work laws should be limited",
        questionId = pack . show $ LimitRightToWorkLaws
      }
  getQuestion JobGuarantee =
    Question
      { questionHeader = "Job Guarantee",
        questionInfo = "A job guarantee (JG) is an economic policy proposal aimed at providing a sustainable solution to the dual problems of inflation and unemployment. Its aim is to create full employment and price stability, by having the state promise to hire unemployed workers as an employer of last resort (ELR).",
        questionSource = "https://en.wikipedia.org/wiki/Job_guarantee",
        questionTopic = "Labor and Welfare",
        questionQuestion = "Anyone who wants to work should be guaranteed a job",
        questionId = pack . show $ JobGuarantee
      }

instance ToJSON LaborAndWelfare

instance FromJSON LaborAndWelfare
