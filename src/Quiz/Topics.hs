{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

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
    inject,
    parseTopics,
    percentageMatch,
    questions,
    topic,
  )
where

import Control.Applicative
import Data.Aeson.Types
import Data.List (filter)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import GHC.Generics

type Topics =
  Education
    + Environment
    + Guns
    + Healthcare
    + Immigration
    + Technology
    + Economics
    + LaborAndWelfare

data a + b = InL a | InR b
  deriving (Show, Eq, Ord, Generic)

infixr 8 +

instance (ToJSON a, ToJSON b) => ToJSON (a + b)

instance (FromJSON a, FromJSON b) => FromJSON (a + b)

instance (ToJSON a, ToJSON b) => ToJSONKey (a + b)

instance (FromJSON a, FromJSON b) => FromJSONKey (a + b)

-- Variant of Data types á la carte
-- http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf

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
-- getTopics @'[Education, Environment, Healthcare]
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
questions = getQuestion <$> topics
  where
    topics =
      getTopics
        @'[ Education,
            Environment,
            Guns,
            Healthcare,
            Immigration,
            Technology,
            Economics,
            LaborAndWelfare
          ]

parseTopic :: forall a. (Injectable a, Bounded a, Enum a) => String -> String -> [Topics]
parseTopic target s = if s == target then getTopics @'[a] else []

parseEducation :: String -> [Topics]
parseEducation = parseTopic @Education "education"

parseEnvironment :: String -> [Topics]
parseEnvironment = parseTopic @Environment "environment"

parseGuns :: String -> [Topics]
parseGuns = parseTopic @Guns "guns"

parseHealthcare :: String -> [Topics]
parseHealthcare = parseTopic @Healthcare "healthcare"

parseImmigration :: String -> [Topics]
parseImmigration = parseTopic @Immigration "immigration"

parseTechnology :: String -> [Topics]
parseTechnology = parseTopic @Technology "technology"

parseEconomics :: String -> [Topics]
parseEconomics = parseTopic @Economics "economics"

parseLaborAndWelfare :: String -> [Topics]
parseLaborAndWelfare = parseTopic @LaborAndWelfare "labor and welfare"

parseTopics :: [String] -> [Topics]
parseTopics =
  foldMap
    ( \s ->
        parseEducation s
          <|> parseEnvironment s
          <|> parseGuns s
          <|> parseHealthcare s
          <|> parseImmigration s
          <|> parseTechnology s
          <|> parseEconomics s
          <|> parseLaborAndWelfare s
    )

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
      { header = "The Green New Deal",
        info = "The Green New Deal is a 10-year plan to mobilize every aspect of American society to 100% clean and renewable energy by 2030, a guaranteed living-wage job for anyone who needs one, and a just transition for both workers and frontline communities.",
        source = "https://www.sunrisemovement.org/green-new-deal",
        questionTopic = "Environment",
        question = "The Green New Deal is a good idea",
        qId = topic GreenNewDeal
      }
  getQuestion NoFossilFuelMoneyPledge =
    Question
      { header = "The No Fossil Fuel Money Pledge",
        info = "I pledge not to take contributions over $200 from oil, gas, and coal industry executives, lobbyists, and PACs and instead prioritize the health of our families, climate, and democracy over fossil fuel industry profits.",
        source = "http://nofossilfuelmoney.org/",
        questionTopic = "Environment",
        question = "It is important to me that my candidate has taken the No Fossil Fuel Money Pledge",
        qId = topic NoFossilFuelMoneyPledge
      }
  getQuestion NuclearPowerToReduceEmissions =
    Question
      { header = "Nuclear Power To Reduce Emissions",
        info = "Nuclear power is a clean and efficient way of boiling water to make steam, which turns turbines to produce electricity. Nuclear power plants use low-enriched uranium fuel to produce electricity through a process called fission—the splitting of uranium atoms in a nuclear reactor. Uranium fuel consists of small, hard ceramic pellets that are packaged into long, vertical tubes. Bundles of this fuel are inserted into the reactor.",
        source = "https://nuclear.gepower.com/company-info/nuclear-power-basics",
        questionTopic = "Environment",
        question = "Leveraging nuclear power is a good way for the U.S. to reduce emissions",
        qId = topic NuclearPowerToReduceEmissions
      }
  getQuestion CarbonTax =
    Question
      { header = "Carbon Tax",
        info = "A carbon tax is a fee that a government imposes on any company that burns fossil fuels. The most widely-discussed are coal, oil, gasoline, and natural gas. When these carbon-rich fuels are burned they produce greenhouse gases. These gases, such as carbon dioxide and methane, create global warming by heating the atmosphere. The resultant climate disruption causes extreme weather such as heat waves, flooding, blizzards, and droughts.",
        source = "https://www.thebalance.com/carbon-tax-definition-how-it-works-4158043",
        questionTopic = "Environment",
        question = "Any company that burns fossil fuels should have to pay a carbon tax",
        qId = topic CarbonTax
      }
  getQuestion ParisAgreement =
    Question
      { header = "The Paris Climate Agreement",
        info = "The Paris Agreement’s central aim is to strengthen the global response to the threat of climate change by keeping a global temperature rise this century well below 2 degrees Celsius above pre-industrial levels and to pursue efforts to limit the temperature increase even further to 1.5 degrees Celsius. Additionally, the agreement aims to increase the ability of countries to deal with the impacts of climate change, and at making finance flows consistent with a low GHG emissions and climate-resilient pathway. To reach these ambitious goals, appropriate mobilization and provision of financial resources, a new technology framework and enhanced capacity-building is to be put in place, thus supporting action by developing countries and the most vulnerable countries, in line with their own national objectives.",
        source = "https://unfccc.int/process-and-meetings/the-paris-agreement/what-is-the-paris-agreement",
        questionTopic = "Environment",
        question = "The U.S. should rejoin the Paris Climate Agreement",
        qId = topic ParisAgreement
      }
  getQuestion BanFracking =
    Question
      { header = "Fracking",
        info = "Fracking is the process of drilling down into the earth before a high-pressure water mixture is directed at the rock to release the gas inside. Water, sand and chemicals are injected into the rock at high pressure which allows the gas to flow out to the head of the well. The process can be carried out vertically or, more commonly, by drilling horizontally to the rock layer, which can create new pathways to release gas or used to extend existing channels.",
        source = "https://www.bbc.com/news/uk-14432401",
        questionTopic = "Environment",
        question = "Fracking should be banned in the United States",
        qId = topic BanFracking
      }
  getQuestion BanOffshoreDrilling =
    Question
      { header = "Offshore Drilling",
        info = "Offshore drilling is the process of extracting petroleum from reserves located beneath the Earth's oceans instead of reserves located on the mainland. Offshore oil rigs have developed greatly over the past years, and have become gigantic structures that house hundreds of people at a time.",
        source = "https://energyeducation.ca/encyclopedia/Offshore_drilling",
        questionTopic = "Environment",
        question = "Offshore drilling should be banned",
        qId = topic BanOffshoreDrilling
      }
  getQuestion DeclareClimateChangeANationalEmergency =
    Question
      { header = "National Emergencies",
        info = "In 1976, Congress passed the National Emergencies Act, which permits the president to pronounce a national emergency when he considers it appropriate. The act offers no specific definition of “emergency” and allows a president to declare one entirely at his or her discretion. By declaring a national emergency, the president avails himself or herself of dozens of specialized laws. Some of these powers have funds the president otherwise could not access.",
        source = "https://www.washingtonpost.com/politics/2019/02/15/what-exactly-is-national-emergency-heres-what-that-means-what-happens-next/",
        questionTopic = "Environment",
        question = "Climate change should be declared a national emergency",
        qId = topic DeclareClimateChangeANationalEmergency
      }

instance Injectable Environment where
  inject = topic

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
      { header = "Universal Background Checks",
        info = "Proposals for universal background checks would require almost all firearms transactions in the United States to be recorded and go through the National Instant Criminal Background Check System (NICS), closing what is sometimes called the private sale exemption.",
        source = "https://en.wikipedia.org/wiki/Universal_background_check",
        questionTopic = "Guns",
        question = "The law should require anyone trying to buy a gun to have a background check done on them",
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
      { header = "Gun Buyback Program",
        info = "Gun buybacks are events where individuals can turn in firearms to law enforcement, usually with no questions asked, and receive some kind of compensation in return. The overall goal of gun buyback programs is to reduce the number of gun deaths and injuries in a community.",
        source = "https://www.gunxgun.org/buybacks_faq",
        questionTopic = "Guns",
        question = "The government should implement a gun buyback program",
        qId = topic GunBuyBack
      }
  getQuestion RequireGunLicense =
    Question
      { header = "Gun Licensing Laws",
        info = "Licensing laws ensure that gun owners have passed a background check before they purchase a gun. In contrast to states which require a background check at the point of sale of a firearm, licensing laws typically require an in-person application at law enforcement agencies, which provides an additional safeguard against fraud or inaccuracies that could allow dangerous individuals to obtain guns.",
        source = "https://lawcenter.giffords.org/gun-laws/policy-areas/gun-owner-responsibilities/licensing/",
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
        info = "In a single payer healthcare system, rather than multiple competing health insurance companies, a single public or quasi-public agency takes responsibility for financing healthcare for all residents. That is, everyone has health insurance under a one health insurance plan, and has access to necessary services — including doctors, hospitals, long-term care, prescription drugs, dentists and vision care. However, individuals may still choose where they receive care. It’s a lot like Medicare, hence the U.S. single payer nickname “Medicare-for-all.",
        source = "https://www.health.harvard.edu/blog/single-payer-healthcare-pluses-minuses-means-201606279835",
        questionTopic = "Healthcare",
        question = "The U.S. should have a single-payer healthcare system",
        qId = topic SinglePayerSystem
      }
  getQuestion PublicHealthInsurance =
    Question
      { header = "Public Health Insurance",
        info = "A program run by U.S. federal, state, or local governments in which people have some or all of their healthcare costs paid for by the government. The two main types of public health insurance are Medicare and Medicaid. Medicare is a federal health insurance program for people aged 65 years or older and people with certain disabilities. Medicaid is a public health insurance program for some individuals and families with a low income or disabilities.",
        source = "https://www.cancer.gov/publications/dictionaries/cancer-terms/def/public-health-insurance",
        questionTopic = "Healthcare",
        question = "The U.S. should have some form of public health insurance",
        qId = topic PublicHealthInsurance
      }
  getQuestion EliminatePrivateHealthInsurance =
    Question
      { header = "Private Health Insurance",
        info = "Private health insurance refers to health insurance plans marketed by the private health insurance industry, as opposed to government-run insurance programs. Private health insurance currently dominates the U.S. health care landscape, covering more than half of the U.S. population.",
        source = "https://www.healthinsurance.org/glossary/private-health-insurance/",
        questionTopic = "Healthcare",
        question = "Private health insurance should be eliminated",
        qId = topic EliminatePrivateHealthInsurance
      }
  getQuestion ImportPrescriptionDrugsFromCanada =
    Question
      { header = "Prescription Drugs in Canada",
        info = "Canadian drugs are cheaper because the government regulates the price of generic and brand-name medication. The prices of cheaper generic drugs, which account for most prescriptions, are set through deals with drug companies at the provincial and national levels.",
        source = "https://www.cbc.ca/news/politics/us-drug-plan-canadian-shortage-1.5232360",
        questionTopic = "Healthcare",
        question = "The U.S. should import some prescription drugs from Canada",
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
      { header = "Potential Effectiveness of a Border Wall",
        info = "Many contemporary examples of border barriers are considered to be effective. Barriers such as the Hungarian border barrier, the Israeli border walls, and the Israeli West Bank barrier have lowered the number of illegal border crossings. In Hungary, for example, the number of illegal immigrants dropped from 4500 per day to 15 after a 175-kilometer long, four-meter high fence was constructed in 2015. On the other hand, research at Texas A&M University and Texas Tech University indicates that the wall, and border walls in general, are unlikely to be effective at reducing illegal immigration or movement of contraband.",
        source = "https://en.wikipedia.org/wiki/Trump_wall#Effectiveness",
        questionTopic = "Immigration",
        question = "The border wall is a good idea",
        qId = topic TrumpBorderWall
      }
  getQuestion TrumpTravelBan =
    Question
      { header = "Trump's Travel Ban",
        info = "The Trump travel ban (sometimes called the \"Muslim ban\") denotes a series of executive actions enacted by Donald Trump as President of the United States in 2017. First, Executive Order 13769 placed stringent restrictions on travel to the United States for citizens of Iran, Iraq, Libya, Somalia, Sudan, Syria, and Yemen. Following protests and legal challenges, a second order, Executive Order 13780, amended some provisions of the first order, and removed Iraq from the list. Finally, Presidential Proclamation 9645 added restrictions on Chad, North Korea, and Venezuela, while Sudan was removed. Six of the eight affected countries are predominantly Muslim.",
        source = "https://en.wikipedia.org/wiki/Trump_travel_ban",
        questionTopic = "Immigration",
        question = "Trump's travel ban was a good idea",
        qId = topic TrumpTravelBan
      }
  getQuestion SupportDACA =
    Question
      { header = "Deferred Action for Childhood Arrivals",
        info = "Deferred Action for Childhood Arrivals (DACA) is a kind of administrative relief from deportation. The purpose of DACA is to protect eligible immigrant youth who came to the United States when they were children from deportation. DACA gives young undocumented immigrants: 1) protection from deportation, and 2) a work permit. The program expires after two years, subject to renewal.",
        source = "https://undocu.berkeley.edu/legal-support-overview/what-is-daca/",
        questionTopic = "Immigration",
        question = "The U.S. should support DACA",
        qId = topic SupportDACA
      }
  getQuestion AllowMoreVisaWorkers =
    Question
      { header = "Work Visas",
        info = "The U.S.A remains one of the most popular immigration destination countries in the world. Employers can apply to employ skilled migrants under various non-immigrant work visa schemes.",
        source = "https://workpermit.com/immigration/usa/us-immigration",
        questionTopic = "Immigration",
        question = "The U.S. should allow more visa workers in",
        qId = topic AllowMoreVisaWorkers
      }
  getQuestion DemilitarizeMexicoUSBorder =
    Question
      { header = "Militarization of the U.S.-Mexico Border",
        info = "The U.S.-Mexico border stands to become one of the world’s most militarized borders, even though the two countries are not at war. Massive increases in appropriations for border security have served only to decrease real security in the U.S., disrupting the quality of life and economies of border communities and eroding human rights in the region.",
        source = "https://www.afsc.org/key-issues/issue/us-mexico-border-militarization",
        questionTopic = "Immigration",
        question = "The U.S.-Mexico border should be demilitarized",
        qId = topic DemilitarizeMexicoUSBorder
      }
  getQuestion InvestInPortsOfEntry =
    Question
      { header = "Ports of Entry",
        info = "In general, a port of entry (POE) is a place where one may lawfully enter a country. It typically has border security staff and facilities to check passports and visas, and inspect luggage to assure that contraband is not imported.",
        source = "https://en.wikipedia.org/wiki/Port_of_entry",
        questionTopic = "Immigration",
        question = "The U.S. should invest in ports of entry",
        qId = topic InvestInPortsOfEntry
      }
  getQuestion AbolishICE =
    Question
      { header = "U.S. Immigration and Customs Enforcement",
        info = "The U.S. Immigration and Customs Enforcement (ICE) is a federal law enforcement agency under the U.S. Department of Homeland Security, principally responsible for immigration and customs enforcement, with additional responsibilities in countering transnational crime.",
        source = "https://en.wikipedia.org/wiki/U.S._Immigration_and_Customs_Enforcement",
        questionTopic = "Immigration",
        question = "ICE should be abolished",
        qId = topic AbolishICE
      }
  getQuestion DecriminalizeIllegalImmigration =
    Question
      { header = "Illegal Immigration",
        info = "Illegal immigration refers to the migration of people into a country in violation of the immigration laws of that country, or the continued residence of people without the legal right to live in that country. Illegal immigration tends to be financially upward, from poorer to richer countries.",
        source = "https://en.wikipedia.org/wiki/Illegal_immigration",
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
        info = "Net neutrality is the idea that internet service providers like Comcast and Verizon should treat all content flowing through their cables and cell towers equally. That means they shouldn't be able to slide some data into “fast lanes” while blocking or otherwise discriminating against other material. In other words, these companies shouldn't be able to block you from accessing a service like Skype, or slow down Netflix or Hulu, in order to encourage you to keep your cable package or buy a different video-streaming service.",
        source = "https://www.wired.com/story/guide-net-neutrality/",
        questionTopic = "Technology",
        question = "Net neutrality should be reinstated",
        qId = topic ReinstateNetNeutrality
      }
  getQuestion DataAsPersonalProperty =
    Question
      { header = "Data as Property",
        info = "During one of the recent Federal Trade Commission (FTC) workshops on competition policy, economist Dennis Carlton and others suggested that defining who has property rights in data is a key to properly analyzing data issues in antitrust and that a natural place to start would be that the individual owns the data including health care information and maybe search data as well. ",
        source = "https://www.forbes.com/sites/washingtonbytes/2018/11/02/privacy-is-not-a-property-right-in-personal-information/#67e580dd280f",
        questionTopic = "Technology",
        question = "Private data should be treated the same as personal property",
        qId = topic DataAsPersonalProperty
      }
  getQuestion CASEAct =
    Question
      { header = "The Copyright Alternative in Small-Claims Enforcement Act",
        info = "The Copyright Alternative in Small-Claims Enforcement Act of 2019 (the CASE Act) is a proposed United States law that would provide a new means for copyright owners to file infringement claims; for copyright users to adjudicate declarations of non-infringement; for owners and users to submit claims related to Section 512(f) of the Digital Millennium Copyright Act; and for would-be defendants to submit counterclaims and legal defenses, such as fair use.",
        source = "https://en.wikipedia.org/wiki/CASE_Act",
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
      { header = "The Estate Tax",
        info = "The Estate Tax is a tax on your right to transfer property at your death. It consists of an accounting of everything you own or have certain interests in at the date of death. The fair market value of these items is used, not necessarily what you paid for them or what their values were when you acquired them.",
        source = "https://www.irs.gov/businesses/small-businesses-self-employed/estate-tax",
        questionTopic = "Economics",
        question = "I am in favor of a national estate tax",
        qId = topic EstateTax
      }
  getQuestion PostalBanking =
    Question
      { header = "Postal Banking",
        info = "...Requiring U.S. postal offices to offer basic financial services to customers, including checking accounts, interest-bearing savings accounts and short-term loans.",
        source = "https://twocents.lifehacker.com/what-is-postal-banking-1828194701",
        questionTopic = "Economics",
        question = "I am in favor of postal banking",
        qId = topic PostalBanking
      }
  getQuestion ReparationsForSlavery =
    Question
      { header = "Reparations for Slavery",
        info = "Reparations for slavery is a political justice concept that argues that reparations should be paid to the descendants of slaves from Sub-Saharan African who were trafficked to and enslaved in the Americas as a consequence of the Atlantic slave trade.",
        source = "https://en.wikipedia.org/wiki/Reparations_for_slavery",
        questionTopic = "Economics",
        question = "American descendants of slaves should receive reparations for slavery",
        qId = topic ReparationsForSlavery
      }
  getQuestion WealthTax =
    Question
      { header = "Wealth Tax",
        info = "It's an annual tax on the net wealth a person holds — so, their assets minus their debts. Not just the income they bring in each year. On the one hand, you can think of it as something like the property taxes people pay on their homes, but applied to all their wealth above a certain level. This is, in fact, a major way that Elizabeth Warren sells her wealth tax in her stump speech. But then, on the other hand, it would be a new type of tax for the federal government to levy — property taxes are usually imposed at the local level, and estate taxes, while on wealth, are only imposed at death.",
        source = "https://www.npr.org/2019/12/05/782135614/how-would-a-wealth-tax-work#what",
        questionTopic = "Economics",
        question = "The U.S. should implement a wealth tax",
        qId = topic WealthTax
      }
  getQuestion BreakingUpLargestBanks =
    Question
      { header = "Breaking Up the Largest Banks",
        info = "The most basic reason to break up the banks is to increase the stability and security of our financial system. Average American consumers and business owners keep their deposits in commercial banks—but in addition to holding deposits, those banks also have divisions that invest in global markets. That exposes banks (and the deposits they hold) to risks in the broader economy.",
        source = "https://www.fundera.com/blog/breaking-up-the-big-banks",
        questionTopic = "Economics",
        question = "The government should break up the largest banks",
        qId = topic BreakingUpLargestBanks
      }
  getQuestion SupportNAFTA =
    Question
      { header = "The North American Free Trade Agreement",
        info = "NAFTA's pros and cons are hotly debated. Critics point to three main disadvantages of NAFTA. First, it sent many U.S. manufacturing jobs to lower-cost Mexico. Second, workers who kept jobs in those industries had to accept lower wages. Third, Mexico's workers suffered exploitation in its maquiladora programs. But NAFTA also has three significant advantages. U.S. grocery prices would be higher without tariff-free imports from Mexico. Imported oil from both Canada and Mexico has prevented higher gas prices. NAFTA has also increased trade and economic growth for all three countries.",
        source = "https://www.thebalance.com/nafta-definition-north-american-free-trade-agreement-3306147",
        questionTopic = "Economics",
        question = "I am in support of NAFTA",
        qId = topic SupportNAFTA
      }

instance Injectable Economics where
  inject = topic

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
      { header = "Minimum Wage",
        info = "The minimum wage in the United States is set by US labor law and a range of state and local laws. Employers generally have to pay workers the highest minimum wage prescribed by federal, state, and local law. Since July 24, 2009, the federal government has mandated a nationwide minimum wage of $7.25 per hour. As of January 2018, there were 29 states with a minimum wage higher than the federal minimum.",
        source = "https://en.wikipedia.org/wiki/Minimum_wage_in_the_United_States",
        questionTopic = "Labor and Welfare",
        question = "The federal minimum wage should be raised",
        qId = topic RaiseMinimumWage
      }
  getQuestion BasicIncome =
    Question
      { header = "Universal Basic Income",
        info = "Basic income, also called universal basic income (UBI), ...is a governmental public program for a periodic payment delivered to all on an individual basis without means test or work requirement",
        source = "https://en.wikipedia.org/wiki/Basic_income",
        questionTopic = "Labor and Welfare",
        question = "U.S. citizens should be guaranteed at least a base level of income",
        qId = topic BasicIncome
      }
  getQuestion PaidFamilyLeave =
    Question
      { header = "Paid Family Leave",
        info = "Paid family leave (PFL) refers to partially or fully compensated time away from work for specific and generally significant family caregiving needs, such as the arrival of a new child or serious illness of a close family member. Although the Family and Medical Leave Act of 1993 (FMLA;P.L. 103-3) provides eligible workers with a federal entitlement to unpaid leave for a limited set of family caregiving needs, no federal law requires private-sector employers to provide paid leave of any kind.",
        source = "https://fas.org/sgp/crs/misc/R44835.pdf",
        questionTopic = "Labor and Welfare",
        question = "Companies should be required to give employees paid family leave",
        qId = topic PaidFamilyLeave
      }
  getQuestion PaidSickLeave =
    Question
      { header = "Paid Sick Leave",
        info = "Currently, there are no federal legal requirements for paid sick leave. For companies subject to the Family and Medical Leave Act (FMLA), the Act does require unpaid sick leave. FMLA provides for up to 12 weeks of unpaid leave for certain medical situations for either the employee or a member of the employee's immediate family. In many instances paid leave may be substituted for unpaid FMLA leave.",
        source = "https://www.dol.gov/general/topic/workhours/sickleave",
        questionTopic = "Labor and Welfare",
        question = "Companies should be required to give employees paid sick leave",
        qId = topic PaidSickLeave
      }
  getQuestion LimitRightToWorkLaws =
    Question
      { header = "Right to Work Laws",
        info = "Right to work laws address situations related to memberships in labor unions -- In short, you never need to join a union or pay union dues to be hired or to work for a company. Commonly, these laws involve employers refusing to hire non-union workers or requiring that workers join a union as a condition of employment. This behavior is illegal because people have the \"right to work\" without being part of a union under state and federal laws.",
        source = "https://employment.findlaw.com/wages-and-benefits/what-are-right-to-work-laws.html",
        questionTopic = "Labor and Welfare",
        question = "Right to work laws should be limited",
        qId = topic LimitRightToWorkLaws
      }
  getQuestion JobGuarantee =
    Question
      { header = "Job Guarantee",
        info = "A job guarantee (JG) is an economic policy proposal aimed at providing a sustainable solution to the dual problems of inflation and unemployment. Its aim is to create full employment and price stability, by having the state promise to hire unemployed workers as an employer of last resort (ELR).",
        source = "https://en.wikipedia.org/wiki/Job_guarantee",
        questionTopic = "Labor and Welfare",
        question = "Anyone who wants to work should be guaranteed a job",
        qId = topic JobGuarantee
      }

instance Injectable LaborAndWelfare where
  inject = topic

instance ToJSON LaborAndWelfare

instance FromJSON LaborAndWelfare
