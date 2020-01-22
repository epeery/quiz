{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Quiz.Candidates
  ( Person,
    Results,
    Respondant (..),
    biden,
    bloomberg,
    booker,
    buttigieg,
    gabbard,
    klobuchar,
    sanders,
    steyer,
    warren,
    yang,
    mostSimilarTo,
  )
where

import Data.Aeson.Types
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import GHC.Generics
import Quiz.Topics

data Respondant r as
  = Respondant
      { rName :: r,
        rAnswers :: as,
        rPic :: Text
      }
  deriving (Show, Eq, Ord, Generic)

instance (FromJSON r, FromJSON as) => FromJSON (Respondant r as)

instance (ToJSON r, ToJSON as) => ToJSON (Respondant r as)

type Person a = Respondant a Positions

type Candidate = Person Text

biden :: Candidate
biden = Respondant "Joe Biden" bidenPositions "https://www.politico.com/interactives/uploads/2020-elections/headshots/png/300/joe-biden.png"

bidenPositions :: Positions
bidenPositions =
  M.fromList
    [ (topic TuitionFreePublicCollege, 0.25), -- Two years of free college
      (topic DebtReliefForStudentLoans, 0.25), -- Only for educators through the Public Service Loan Forgiveness Program
      (topic AffirmativeAction, 1),
      (topic UniversalChildCare, 1),
      (topic UniversalPreKindergarten, 1),
      (topic IncreaseFundingForPublicEducation, 1),
      (topic GreenNewDeal, 0.5),
      (topic NoFossilFuelMoneyPledge, 1),
      (topic NuclearPowerToReduceEmissions, 1),
      (topic ParisAgreement, 1),
      (topic BanOffshoreDrilling, 0.25), -- Specificly in the Arctic
      (topic UniversalBackgroundChecks, 1),
      (topic BanAssaultWeapons, 1),
      (topic GunBuyBack, 0.5), -- Voluntary
      (topic RequireGunLicense, -0.5),
      (topic SinglePayerSystem, -1),
      (topic PublicHealthInsurance, 1),
      (topic EliminatePrivateHealthInsurance, -1),
      (topic TrumpBorderWall, -1),
      (topic TrumpTravelBan, -1),
      (topic SupportDACA, 1),
      (topic AllowMoreVisaWorkers, 1),
      (topic AbolishICE, -1),
      (topic DecriminalizeIllegalImmigration, -1)
    ]

bloomberg :: Candidate
bloomberg = Respondant "Michael Bloomberg" bloombergPositions "https://www.politico.com/interactives/uploads/2020-elections/headshots/png/300/michael-bloomberg.png"

bloombergPositions :: Positions
bloombergPositions =
  M.fromList
    [ (topic IncreaseFundingForPublicEducation, 1),
      (topic GreenNewDeal, -1),
      (topic NoFossilFuelMoneyPledge, -1),
      (topic NuclearPowerToReduceEmissions, 0.25),
      (topic CarbonTax, 1),
      (topic ParisAgreement, 1),
      (topic BanFracking, -1),
      (topic BanOffshoreDrilling, -1),
      (topic UniversalBackgroundChecks, 1),
      (topic BanAssaultWeapons, 1),
      (topic GunBuyBack, 0.5), -- Voluntary
      (topic RequireGunLicense, 1),
      (topic SinglePayerSystem, -1),
      (topic EliminatePrivateHealthInsurance, -1),
      (topic ImportPrescriptionDrugsFromCanada, 0.25),
      (topic TrumpBorderWall, -1),
      (topic TrumpTravelBan, -1)
    ]

booker :: Candidate
booker = Respondant "Cory Booker" bookerPositions "https://www.politico.com/interactives/uploads/2020-elections/headshots/png/300/cory-booker.png"

bookerPositions :: Positions
bookerPositions =
  M.fromList
    [ (topic TuitionFreePublicCollege, -0.5),
      (topic DebtReliefForStudentLoans, 0.25), -- Only for educators through the Public Service Loan Forgiveness Program
      (topic AffirmativeAction, 1),
      (topic GreenNewDeal, 1),
      (topic NoFossilFuelMoneyPledge, 1),
      (topic NuclearPowerToReduceEmissions, 1),
      (topic CarbonTax, 1),
      (topic ParisAgreement, 1),
      (topic BanFracking, 1),
      (topic DeclareClimateChangeANationalEmergency, 1),
      (topic UniversalBackgroundChecks, 1),
      (topic BanAssaultWeapons, 1),
      (topic GunBuyBack, 1), -- Mandatory
      (topic RequireGunLicense, 1),
      (topic SinglePayerSystem, 1),
      (topic EliminatePrivateHealthInsurance, -1),
      (topic ImportPrescriptionDrugsFromCanada, -1),
      (topic TrumpBorderWall, -1),
      (topic TrumpTravelBan, -1),
      (topic SupportDACA, 1),
      (topic AbolishICE, -1)
    ]

buttigieg :: Candidate
buttigieg = Respondant "Pete Buttigieg" buttigiegPositions "https://www.politico.com/interactives/uploads/2020-elections/headshots/png/300/pete-buttigieg.png"

buttigiegPositions :: Positions
buttigiegPositions =
  M.fromList
    [ (topic TuitionFreePublicCollege, 0.5), -- Only for lower and middle income families
      (topic AffirmativeAction, 1),
      (topic UniversalChildCare, 1),
      (topic UniversalPreKindergarten, 1),
      (topic IncreaseFundingForPublicEducation, 1),
      (topic GreenNewDeal, 1),
      (topic NoFossilFuelMoneyPledge, 1),
      (topic NuclearPowerToReduceEmissions, 1),
      (topic CarbonTax, 1),
      (topic ParisAgreement, 1),
      (topic BanFracking, 1),
      (topic UniversalBackgroundChecks, 1),
      (topic BanAssaultWeapons, 1),
      (topic GunBuyBack, 0.5), -- Voluntary
      (topic RequireGunLicense, 1),
      (topic SinglePayerSystem, -1),
      (topic PublicHealthInsurance, 1),
      (topic EliminatePrivateHealthInsurance, -1),
      (topic ImportPrescriptionDrugsFromCanada, 1),
      (topic TrumpBorderWall, -1),
      (topic TrumpTravelBan, -1),
      (topic SupportDACA, 1),
      (topic AllowMoreVisaWorkers, 1),
      (topic DemilitarizeMexicoUSBorder, 1),
      (topic AbolishICE, -1),
      (topic DecriminalizeIllegalImmigration, 1)
    ]

gabbard :: Candidate
gabbard = Respondant "Tulsi Gabbard" gabbardPositions "https://www.politico.com/interactives/uploads/2020-elections/headshots/png/300/tulsi-gabbard.png"

gabbardPositions :: Positions
gabbardPositions =
  M.fromList
    [ (topic TuitionFreePublicCollege, 1),
      (topic DebtReliefForStudentLoans, 1),
      (topic GreenNewDeal, 1),
      (topic NoFossilFuelMoneyPledge, 1),
      (topic NuclearPowerToReduceEmissions, -1),
      (topic ParisAgreement, 1),
      (topic BanFracking, 1),
      (topic BanOffshoreDrilling, 1),
      (topic DeclareClimateChangeANationalEmergency, 1),
      (topic UniversalBackgroundChecks, 1),
      (topic BanAssaultWeapons, 1),
      (topic SinglePayerSystem, 1),
      (topic EliminatePrivateHealthInsurance, -1),
      (topic TrumpBorderWall, -1),
      (topic TrumpTravelBan, -1),
      (topic SupportDACA, 1),
      (topic AbolishICE, 1)
    ]

klobuchar :: Candidate
klobuchar = Respondant "Amy Klobuchar" klobucharPositions "https://www.politico.com/interactives/uploads/2020-elections/headshots/png/300/amy-klobuchar.png"

klobucharPositions :: Positions
klobucharPositions =
  M.fromList
    [ (topic TuitionFreePublicCollege, 0.5),
      (topic DebtReliefForStudentLoans, -1),
      (topic UniversalChildCare, 1),
      (topic GreenNewDeal, 1),
      (topic NoFossilFuelMoneyPledge, 1),
      (topic NuclearPowerToReduceEmissions, 1),
      (topic CarbonTax, 0.25),
      (topic ParisAgreement, 0.25),
      (topic BanFracking, -1),
      (topic DeclareClimateChangeANationalEmergency, 1),
      (topic UniversalBackgroundChecks, 1),
      (topic BanAssaultWeapons, 1),
      (topic GunBuyBack, 0.5), -- Voluntary
      (topic SinglePayerSystem, -1),
      (topic PublicHealthInsurance, 1),
      (topic EliminatePrivateHealthInsurance, -1),
      (topic ImportPrescriptionDrugsFromCanada, 1),
      (topic TrumpBorderWall, -1),
      (topic TrumpTravelBan, -1),
      (topic SupportDACA, 1),
      (topic InvestInPortsOfEntry, 1),
      (topic AbolishICE, -1)
    ]

sanders :: Candidate
sanders = Respondant "Bernie Sanders" sandersPositions "https://www.politico.com/interactives/uploads/2020-elections/headshots/png/300/bernie-sanders.png"

sandersPositions :: Positions
sandersPositions =
  M.fromList
    [ (topic TuitionFreePublicCollege, 1),
      (topic DebtReliefForStudentLoans, 1),
      (topic AffirmativeAction, 1),
      (topic UniversalChildCare, 1),
      (topic UniversalPreKindergarten, 1),
      (topic IncreaseFundingForPublicEducation, 1),
      (topic GreenNewDeal, 1),
      (topic NoFossilFuelMoneyPledge, 1),
      (topic NuclearPowerToReduceEmissions, -1),
      (topic CarbonTax, 1),
      (topic ParisAgreement, 1),
      (topic BanFracking, 1),
      (topic BanOffshoreDrilling, 1),
      (topic DeclareClimateChangeANationalEmergency, 1),
      (topic UniversalBackgroundChecks, 1),
      (topic BanAssaultWeapons, 1),
      (topic GunBuyBack, 0.5), -- Voluntary
      (topic RequireGunLicense, 0.5), -- Only for assault weapons
      (topic SinglePayerSystem, 1),
      (topic EliminatePrivateHealthInsurance, 1),
      (topic ImportPrescriptionDrugsFromCanada, 1),
      (topic TrumpBorderWall, -1),
      (topic TrumpTravelBan, -1),
      (topic SupportDACA, 1),
      (topic AllowMoreVisaWorkers, 1),
      (topic DemilitarizeMexicoUSBorder, 1),
      (topic InvestInPortsOfEntry, 1),
      (topic AbolishICE, 1),
      (topic DecriminalizeIllegalImmigration, 1)
    ]

steyer :: Candidate
steyer = Respondant "Tom Steyer" steyerPositions "https://www.politico.com/interactives/uploads/2020-elections/headshots/png/300/tom-steyer.png"

steyerPositions :: Positions
steyerPositions =
  M.fromList
    [ (topic TuitionFreePublicCollege, 0.5), -- Community college
      (topic GreenNewDeal, 1),
      (topic NoFossilFuelMoneyPledge, 1),
      (topic NuclearPowerToReduceEmissions, -1),
      (topic CarbonTax, 1),
      (topic ParisAgreement, 1),
      (topic BanFracking, 1),
      (topic BanOffshoreDrilling, 1),
      (topic UniversalBackgroundChecks, 1),
      (topic BanAssaultWeapons, 1),
      (topic GunBuyBack, 0.5), -- Voluntary
      (topic RequireGunLicense, 1),
      (topic SinglePayerSystem, 1),
      (topic EliminatePrivateHealthInsurance, -1),
      (topic TrumpBorderWall, -1),
      (topic TrumpTravelBan, -1)
    ]

warren :: Candidate
warren = Respondant "Elizabeth Warren" warrenPositions "https://www.politico.com/interactives/uploads/2020-elections/headshots/png/300/elizabeth-warren.png"

warrenPositions :: Positions
warrenPositions =
  M.fromList
    [ (topic TuitionFreePublicCollege, 1),
      (topic DebtReliefForStudentLoans, 1),
      (topic UniversalChildCare, 1),
      (topic UniversalPreKindergarten, 1),
      (topic GreenNewDeal, 1),
      (topic NoFossilFuelMoneyPledge, 1),
      (topic CarbonTax, 1),
      (topic ParisAgreement, 1),
      (topic BanFracking, 1),
      (topic BanOffshoreDrilling, 1),
      (topic DeclareClimateChangeANationalEmergency, 1),
      (topic UniversalBackgroundChecks, 1),
      (topic BanAssaultWeapons, 1),
      (topic GunBuyBack, 0.5), -- Voluntary
      (topic RequireGunLicense, 1),
      (topic SinglePayerSystem, 1),
      (topic EliminatePrivateHealthInsurance, 1),
      (topic ImportPrescriptionDrugsFromCanada, 1),
      (topic TrumpBorderWall, -1),
      (topic TrumpTravelBan, -1),
      (topic SupportDACA, 1),
      (topic AbolishICE, -1),
      (topic DecriminalizeIllegalImmigration, 1)
    ]

yang :: Candidate
yang = Respondant "Andrew Yang" yangPositions "https://www.politico.com/interactives/uploads/2020-elections/headshots/png/300/andrew-yang.png"

yangPositions :: Positions
yangPositions =
  M.fromList
    [ (topic TuitionFreePublicCollege, 0.5), -- Community college
      (topic DebtReliefForStudentLoans, 1),
      (topic AffirmativeAction, 1),
      (topic UniversalChildCare, 1),
      (topic UniversalPreKindergarten, 1),
      (topic IncreaseFundingForPublicEducation, 1),
      (topic GreenNewDeal, 0.5),
      (topic NoFossilFuelMoneyPledge, 1),
      (topic NuclearPowerToReduceEmissions, 1),
      (topic CarbonTax, 1),
      (topic ParisAgreement, 1),
      (topic BanFracking, 0.5),
      (topic BanOffshoreDrilling, 1),
      (topic DeclareClimateChangeANationalEmergency, 1),
      (topic UniversalBackgroundChecks, 1),
      (topic BanAssaultWeapons, 1),
      (topic GunBuyBack, 0.5), -- Voluntary
      (topic RequireGunLicense, 1),
      (topic SinglePayerSystem, 1),
      (topic PublicHealthInsurance, 1),
      (topic EliminatePrivateHealthInsurance, -1),
      (topic ImportPrescriptionDrugsFromCanada, 0.5),
      (topic TrumpBorderWall, -1),
      (topic TrumpTravelBan, -1),
      (topic SupportDACA, 1),
      (topic AllowMoreVisaWorkers, 1),
      (topic InvestInPortsOfEntry, 1),
      (topic AbolishICE, -1),
      (topic DecriminalizeIllegalImmigration, 0.75) -- Still should work to combat drug and human trafficking but that's it
    ]

-- Ordered in terms of popularity.
-- The order this list only affects the the ordering of ties.
candidates :: [Candidate]
candidates =
  [ biden,
    sanders,
    warren,
    buttigieg,
    bloomberg,
    yang,
    klobuchar,
    -- booker, -- Dropped out
    gabbard,
    steyer
  ]

comparePeople :: Person a -> Person b -> Double
-- comparePeople (Respondant _ p1) (Respondant _ p2) = comparePositions p1 p2
comparePeople (Respondant _ p1 _) (Respondant _ p2 _) = percentageMatch p1 p2

type Results = [(Candidate, Double)]

-- Temporary way to rank candidates in terms of similarity to an arbitrary person
-- Not super great in terms of time complexity either
mostSimilarTo :: Person a -> Results
mostSimilarTo person = sortOn snd $ f person <$> candidates
  where
    f person' candidate = (candidate, comparePeople person' candidate)
