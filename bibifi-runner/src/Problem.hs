module Problem (
      ProblemRunner(..)
    , contestToProblemRunner
    , module Export
    ) where

import qualified Data.Text as Text
import Model

import Common
import Problem.Class as Export

-- Import additional contest specification instances here.
import Problem.ATM (ATMSpec(..))
import Problem.ArtGallery (ArtGallery(..))
import Problem.EHR (EHRSpec(..))

data ProblemRunner = forall a . (ExtractContest a, ProblemRunnerClass a) => ProblemRunner a

contestToProblemRunner :: Entity Contest -> ProblemRunner
contestToProblemRunner contestE = helper $ contestUrl $ entityVal contestE
    where 
        helper "spring2015coursera" = ProblemRunner $ ArtGallery contestE
        helper "fall2015coursera" = ProblemRunner $ ATMSpec contestE
        helper "fall2015" = ProblemRunner $ ATMSpec contestE
        helper "fall2016" = ProblemRunner $ EHRSpec contestE
        helper url = error $ "You must define Core.Modular.toModular for url: " ++ (Text.unpack url)
