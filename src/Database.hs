{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Monad (void)
import Data.Int (Int64)
import Database.MySQL.Simple
    ( execute, query, query_, Connection, Only(Only) )
import Types ( User(..), Author(..), Software(..), License(..) )
import Data.Text (Text)

class Entity a where
  insert     :: Connection -> a -> IO Int64
  update     :: Connection -> a -> IO ()
  findById   :: Connection -> Int -> IO (Maybe a)
  findAll    :: Connection -> IO [a]
  deleteById :: Connection -> Int -> IO ()

instance Entity Software where
  insert conn sw = do
    _ <- execute conn "INSERT INTO software (name, annotation, kind, version, distribution_location, license_id) VALUES (?, ?, ?, ?, ?, ?)"
                   (swName sw, swAnnotation sw, swKind sw, swVersion sw, swDistroLocation sw, swLicenseId sw)
    [Only lastId] <- query_ conn "SELECT LAST_INSERT_ID()"
    return lastId

  update conn sw =
    void $ execute conn "UPDATE software SET name = ?, annotation = ?, kind = ?, version = ?, distribution_location = ?, license_id = ? WHERE software_id = ?"
                   (swName sw, swAnnotation sw, swKind sw, swVersion sw, swDistroLocation sw, swLicenseId sw, swId sw)

  findById conn softwareId = do
    res <- query conn "SELECT software_id, name, annotation, kind, version, distribution_location, license_id FROM software WHERE software_id = ?" (Only softwareId)
    return $ case res of
      [ (swId', name, ann, kind, ver, loc, licId) ] -> Just $ Software swId' name ann kind ver loc licId
      _ -> Nothing

  findAll conn = do
    res <- query_ conn "SELECT software_id, name, annotation, kind, version, distribution_location, license_id FROM software"
    return $ map (\(swId', name, ann, kind, ver, loc, licId) -> Software swId' name ann kind ver loc licId) res

  deleteById conn softwareId =
    void $ execute conn "DELETE FROM software WHERE software_id = ?" (Only softwareId)

instance Entity Author where
  insert conn author = do
    _ <- execute conn "INSERT INTO authors (full_name, contact_info) VALUES (?, ?)"
                   (authorFullName author, authorContact author)
    [Only lastId] <- query_ conn "SELECT LAST_INSERT_ID()"
    return lastId

  update conn author =
    void $ execute conn "UPDATE authors SET full_name = ?, contact_info = ? WHERE author_id = ?"
                   (authorFullName author, authorContact author, authorId author)

  findById conn authorId' = do
    res <- query conn "SELECT author_id, full_name, contact_info FROM authors WHERE author_id = ?" (Only authorId')
    return $ case res of
      [ (id', name, contact) ] -> Just $ Author id' name contact
      _ -> Nothing

  findAll conn = do
    res <- query_ conn "SELECT author_id, full_name, contact_info FROM authors"
    return $ map (\(id', name, contact) -> Author id' name contact) res

  deleteById conn authorId' =
    void $ execute conn "DELETE FROM authors WHERE author_id = ?" (Only authorId')

instance Entity License where
  insert conn lic = do
    _ <- execute conn "INSERT INTO licenses (usage_terms, usage_conditions) VALUES (?, ?)"
                   (licenseTerms lic, licenseConditions lic)
    [Only lastId] <- query_ conn "SELECT LAST_INSERT_ID()"
    return lastId

  update conn lic =
    void $ execute conn "UPDATE licenses SET usage_terms = ?, usage_conditions = ? WHERE license_id = ?"
                   (licenseTerms lic, licenseConditions lic, licenseId lic)

  findById conn licenseId' = do
    res <- query conn "SELECT license_id, usage_terms, usage_conditions FROM licenses WHERE license_id = ?" (Only licenseId')
    return $ case res of
      [ (id', terms, cond) ] -> Just $ License id' terms cond
      _ -> Nothing

  findAll conn = do
    res <- query_ conn "SELECT license_id, usage_terms, usage_conditions FROM licenses"
    return $ map (\(id', terms, cond) -> License id' terms cond) res

  deleteById conn licenseId' =
    void $ execute conn "DELETE FROM licenses WHERE license_id = ?" (Only licenseId')

instance Entity User where
  insert conn user = do
    _ <- execute conn "INSERT INTO users (user_name, faculty_department) VALUES (?, ?)"
                   (userName user, userFacultyDept user)
    [Only lastId] <- query_ conn "SELECT LAST_INSERT_ID()"
    return lastId

  update conn user =
    void $ execute conn "UPDATE users SET user_name = ?, faculty_department = ? WHERE user_id = ?"
                   (userName user, userFacultyDept user, userId user)

  findById conn userId' = do
    res <- query conn "SELECT user_id, user_name, faculty_department FROM users WHERE user_id = ?" (Only userId')
    return $ case res of
      [ (id', name, dept) ] -> Just $ User id' name dept
      _ -> Nothing

  findAll conn = do
    res <- query_ conn "SELECT user_id, user_name, faculty_department FROM users"
    return $ map (\(id', name, dept) -> User id' name dept) res

  deleteById conn userId' =
    void $ execute conn "DELETE FROM users WHERE user_id = ?" (Only userId')

linkSoftwareToAuthor :: Connection -> Int -> Int -> IO ()
linkSoftwareToAuthor conn softwareId_ authorId_ =
  void $ execute conn "INSERT INTO software_authors (software_id, author_id) VALUES (?, ?)"
                 (softwareId_, authorId_)

unlinkSoftwareFromAuthor :: Connection -> Int -> Int -> IO ()
unlinkSoftwareFromAuthor conn softwareId_ authorId_ =
  void $ execute conn "DELETE FROM software_authors WHERE software_id = ? AND author_id = ?"
                 (softwareId_, authorId_)

findAuthorsForSoftware :: Connection -> Int -> IO [Author]
findAuthorsForSoftware conn softwareId_ = do
  res <- query conn "SELECT a.author_id, a.full_name, a.contact_info FROM authors a JOIN software_authors sa ON a.author_id = sa.author_id WHERE sa.software_id = ?" (Only softwareId_)
  return $ map (\(id', name, contact) -> Author id' name contact) res

data SoftwarePopularity = SoftwarePopularity
  { popSwName :: Text
  , popUsageCount :: Int
  } deriving (Show)

recordUsage :: Connection -> Int -> Int -> IO ()
recordUsage conn softwareId_ userId_ =
  void $ execute conn "INSERT INTO usage_statistics (software_id, user_id) VALUES (?, ?)"
                 (softwareId_, userId_)

getPopularityReport :: Connection -> IO [SoftwarePopularity]
getPopularityReport conn = do
  res <- query_ conn "SELECT s.name, COUNT(us.stat_id) AS usage_count \
                     \FROM software s \
                     \JOIN usage_statistics us ON s.software_id = us.software_id \
                     \GROUP BY s.software_id \
                     \ORDER BY usage_count DESC"
  return $ map (uncurry SoftwarePopularity) res
