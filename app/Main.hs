{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Control.Monad (when)
import Data.Text (pack, unpack)
import Database.MySQL.Simple
    ( Connection,
      close,
      connect,
      defaultConnectInfo,
      ConnectInfo(connectDatabase, connectHost, connectUser,
                  connectPassword) )
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Database
    ( findAuthorsForSoftware,
      getPopularityReport,
      linkSoftwareToAuthor,
      recordUsage,
      Entity(..),
      SoftwarePopularity(SoftwarePopularity) )
import Types
    ( User(User),
      Author(Author),
      Software(Software, swDistroLocation, swName, swVersion,
               swAnnotation, swKind),
      License(License) )

main :: IO ()
main = do
  conn <- connectToDB
  putStrLn "Successfully connected to the database!"
  mainLoop conn
  close conn
  putStrLn "Exiting program."

mainLoop :: Connection -> IO ()
mainLoop conn = do
  printMenu
  choice <- getLine
  continue <- handleChoice conn choice
  when continue $ mainLoop conn

handleChoice :: Connection -> String -> IO Bool
handleChoice conn choice = case choice of
  "1" -> addSoftware conn >> return True
  "2" -> showAllSoftware conn >> return True
  "3" -> updateSoftwareHandler conn >> return True
  "4" -> deleteEntity @Software conn "Software" >> return True

  "5" -> addAuthor conn >> return True
  "6" -> showAllAuthors conn >> return True
  "7" -> deleteEntity @Author conn "Author" >> return True

  "8" -> addUser conn >> return True
  "9" -> showAllUsers conn >> return True
  "10" -> deleteEntity @User conn "User" >> return True

  "11" -> addLicense conn >> return True
  "12" -> showAllLicenses conn >> return True
  "13" -> deleteEntity @License conn "License" >> return True

  "14" -> linkAuthorToSoftwareHandler conn >> return True
  "15" -> findAuthorsForSoftwareHandler conn >> return True

  "16" -> recordUsageHandler conn >> return True
  "17" -> showPopularityReportHandler conn >> return True

  "18" -> return False
  _   -> putStrLn "Invalid choice. Please try again." >> return True

addSoftware :: Connection -> IO ()
addSoftware conn = do
  putStrLn "\n--- Add New Software ---"
  licId <- promptForInt "Enter License ID for the new software"  

  maybeLicense <- findById conn licId :: IO (Maybe License)

  case maybeLicense of
    Nothing ->
      putStrLn $ "Error: License with ID " ++ show licId ++ " does not exist. Please add it first."
    Just _ -> do
      putStrLn "License found. Please provide software details:"
      name <- prompt "Name"
      ver <- prompt "Version"
      ann <- prompt "Annotation"
      kind <- prompt "Kind"
      loc <- prompt "Distribution Location"
      
      let newSw = Software 0 (pack name) (pack ann) (pack kind) (pack ver) (pack loc) licId
      newId <- insert conn newSw
      putStrLn $ "Successfully added Software with ID: " ++ show newId

showAllSoftware :: Connection -> IO ()
showAllSoftware conn = do
  putStrLn "\n--- Software List ---"
  allSoftware <- findAll conn :: IO [Software]
  if null allSoftware
    then putStrLn "Software list is empty."
    else mapM_ print allSoftware

updateSoftwareHandler :: Connection -> IO ()
updateSoftwareHandler conn = do
  putStrLn "\n--- Edit Software Information ---"
  softwareId <- promptForInt "Enter Software ID to edit"
  maybeSw <- findById conn softwareId
  case maybeSw of
    Nothing -> putStrLn $ "Software with ID " ++ show softwareId ++ " not found."
    Just oldSw -> do
      putStrLn "Enter new values. Press Enter to keep the current value."
      name <- promptWithDefault "Name" (unpack $ swName oldSw)
      ver <- promptWithDefault "Version" (unpack $ swVersion oldSw)
      ann <- promptWithDefault "Annotation" (unpack $ swAnnotation oldSw)
      kind <- promptWithDefault "Kind" (unpack $ swKind oldSw)
      loc <- promptWithDefault "Location" (unpack $ swDistroLocation oldSw)
      let updatedSw = oldSw { swName = pack name, swVersion = pack ver, swAnnotation = pack ann, swKind = pack kind, swDistroLocation = pack loc }
      update conn updatedSw
      putStrLn "Software information updated successfully."

addAuthor :: Connection -> IO ()
addAuthor conn = do
  putStrLn "\n--- Add New Author ---"
  name <- prompt "Full Name"
  contact <- prompt "Contact Info"
  let newAuthor = Author 0 (pack name) (pack contact)
  newId <- insert conn newAuthor
  putStrLn $ "Successfully added Author with ID: " ++ show newId

showAllAuthors :: Connection -> IO ()
showAllAuthors conn = do
  putStrLn "\n--- Author List ---"
  allAuthors <- findAll conn :: IO [Author]
  if null allAuthors
    then putStrLn "Author list is empty."
    else mapM_ print allAuthors

addUser :: Connection -> IO ()
addUser conn = do
    putStrLn "\n--- Add New User ---"
    name <- prompt "Username"
    dept <- prompt "Faculty/Department"
    let newUser = User 0 (pack name) (pack dept)
    newId <- insert conn newUser
    putStrLn $ "Successfully added User with ID: " ++ show newId

showAllUsers :: Connection -> IO ()
showAllUsers conn = do
    putStrLn "\n--- User List ---"
    allUsers <- findAll conn :: IO [User]
    if null allUsers
      then putStrLn "User list is empty."
      else mapM_ print allUsers

addLicense :: Connection -> IO ()
addLicense conn = do
    putStrLn "\n--- Add New License ---"
    terms <- prompt "Usage Terms (e.g., 'Annual Subscription')"
    conditions <- prompt "Conditions (e.g., 'For educational purposes only')"
    let newLicense = License 0 (pack terms) (pack conditions)
    newId <- insert conn newLicense
    putStrLn $ "Successfully added License with ID: " ++ show newId

showAllLicenses :: Connection -> IO ()
showAllLicenses conn = do
    putStrLn "\n--- License List ---"
    allLicenses <- findAll conn :: IO [License]
    if null allLicenses
      then putStrLn "License list is empty."
      else mapM_ print allLicenses

deleteEntity :: forall a. (Entity a, Show a) => Connection -> String -> IO ()
deleteEntity conn entityName = do
  putStrLn $ "\n--- Delete " ++ entityName ++ " ---"
  entityId <- promptForInt "Enter ID to delete"
  maybeEntity <- findById conn entityId :: IO (Maybe a)
  case maybeEntity of
    Nothing -> putStrLn $ "Record with ID " ++ show entityId ++ " not found."
    Just entity -> do
      print entity
      confirm <- prompt "Are you sure? (y/n)"
      when (confirm == "y") $ do
        deleteById @a conn entityId
        putStrLn "Record deleted successfully."

linkAuthorToSoftwareHandler :: Connection -> IO ()
linkAuthorToSoftwareHandler conn = do
  putStrLn "\n--- Link Author to Software ---"
  softwareId <- promptForInt "Enter Software ID"
  authId <- promptForInt "Enter Author ID"
  linkSoftwareToAuthor conn softwareId authId
  putStrLn "Link created successfully!"

findAuthorsForSoftwareHandler :: Connection -> IO ()
findAuthorsForSoftwareHandler conn = do
  putStrLn "\n--- Find Authors for Software ---"
  softwareId <- promptForInt "Enter Software ID"
  authors <- findAuthorsForSoftware conn softwareId
  if null authors
    then putStrLn $ "No authors found for Software with ID " ++ show softwareId ++ "."
    else mapM_ print authors

recordUsageHandler :: Connection -> IO ()
recordUsageHandler conn = do
  putStrLn "\n--- Record Software Usage ---"
  softwareId <- promptForInt "Enter Software ID"
  userId' <- promptForInt "Enter User ID"
  recordUsage conn softwareId userId'
  putStrLn "Usage recorded successfully!"

showPopularityReportHandler :: Connection -> IO ()
showPopularityReportHandler conn = do
  putStrLn "\n--- Software Popularity Report ---"
  report <- getPopularityReport conn
  if null report
    then putStrLn "No usage data available."
    else do
      putStrLn "Software Name\t\tUsage Count"
      putStrLn "---------------------------------------------------"
      mapM_ (\(SoftwarePopularity name count) -> putStrLn $ unpack name ++ "\t\t" ++ show count) report

connectToDB :: IO Connection
connectToDB = connect defaultConnectInfo { connectHost = "127.0.0.1", connectUser = "user", connectPassword = "password", connectDatabase = "faculty_software" }

printMenu :: IO ()
printMenu = do
  putStrLn "\n================ Main Menu ================"
  putStrLn "--- Software Management ---"
  putStrLn " 1. Add Software"
  putStrLn " 2. Show All Software"
  putStrLn " 3. Edit Software"
  putStrLn " 4. Delete Software"
  putStrLn "--- Author Management ---"
  putStrLn " 5. Add Author"
  putStrLn " 6. Show All Authors"
  putStrLn " 7. Delete Author"
  putStrLn "--- User Management ---"
  putStrLn " 8. Add User"
  putStrLn " 9. Show All Users"
  putStrLn " 10. Delete User"
  putStrLn "--- License Management ---"
  putStrLn " 11. Add License"
  putStrLn " 12. Show All Licenses"
  putStrLn " 13. Delete License"
  putStrLn "--- Relations & Statistics ---"
  putStrLn " 14. Link Author to Software"
  putStrLn " 15. Find Authors for Software"
  putStrLn " 16. Record Software Usage"
  putStrLn " 17. Show Popularity Report"
  putStrLn "------------------------------------"
  putStrLn " 18. Exit"
  putStr "--> Your choice: "
  hFlush stdout

prompt :: String -> IO String
prompt text = putStr (text ++ ": ") >> hFlush stdout >> getLine

promptWithDefault :: String -> String -> IO String
promptWithDefault text def = do
  putStr (text ++ " [" ++ def ++ "]: ")
  hFlush stdout
  input <- getLine
  return $ if null input then def else input

promptForInt :: String -> IO Int
promptForInt text = do
  str <- prompt text
  case readMaybe str of
    Just num -> return num
    Nothing  -> putStrLn "Error: a number is required." >> promptForInt text
