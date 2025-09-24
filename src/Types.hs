{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data License = License
  { licenseId :: Int
  , licenseTerms :: Text
  , licenseConditions :: Text
  } deriving (Show, Eq, Generic)

data Software = Software
  { swId :: Int
  , swName :: Text
  , swAnnotation :: Text
  , swKind :: Text
  , swVersion :: Text
  , swDistroLocation :: Text
  , swLicenseId :: Int
  } deriving (Show, Eq, Generic)

data Author = Author
  { authorId :: Int
  , authorFullName :: Text
  , authorContact :: Text
  } deriving (Show, Eq, Generic)

data SoftwareAuthor = SoftwareAuthor
  { saSoftwareId :: Int
  , saAuthorId :: Int
  } deriving (Show, Eq, Generic)

data User = User
  { userId :: Int
  , userName :: Text
  , userFacultyDept :: Text
  } deriving (Show, Eq, Generic)

data UsageStatistic = UsageStatistic
  { statId :: Int
  , statSoftwareId :: Int
  , statUserId :: Int
  , statAccessTimestamp :: UTCTime
  , statUsageCount :: Int
  } deriving (Show, Eq, Generic)
