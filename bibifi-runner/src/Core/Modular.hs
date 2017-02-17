module Core.Modular (
      toModular
    , getModContestId
    , ModContest(..)
    , module Export
    ) where

-- import Control.Monad.IO.Class
import qualified Data.Text as Text
import Model

import Common
import Core.Modular.Class as Export

-- Import additional contest specification instances here.
import Core.Modular.ATM (ATMSpec(..))
import Core.Modular.ArtGallery (ArtGallery(..))
import Core.Modular.EHR (EHRSpec(..))

-- This function defines which code should be run for a given contest. 
-- Define this for future contests, and make an instance of ModularContest as needed. 
toModular :: Entity Contest -> ModContest
toModular contestE = helper $ contestUrl $ entityVal contestE
    where
        -- helper "spring2015coursera" = CourseraContest' $ Coursera contestE
        helper "spring2015coursera" = ArtContest $ ArtGallery contestE
        helper "fall2015coursera" = ATMContest $ ATMSpec contestE
        helper "fall2015" = ATMContest $ ATMSpec contestE
        helper "fall2016" = EHRContest $ EHRSpec contestE
        helper url = error $ "You must define Core.Modular.toModular for url: " ++ (Text.unpack url)

data ModContest = 
      ATMContest ATMSpec
    | ArtContest ArtGallery
    | EHRContest EHRSpec

getModContestId :: ModContest -> ContestId
getModContestId (ATMContest (ATMSpec (Entity cId _))) = cId
getModContestId (ArtContest (ArtGallery (Entity cId _))) = cId
getModContestId (EHRContest (EHRSpec (Entity cId _))) = cId
