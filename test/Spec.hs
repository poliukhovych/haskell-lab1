{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Test.Hspec
import Database.MySQL.Simple
import Control.Exception (bracket)
import Control.Monad (void, replicateM_)

import Database 
import Types    

connectToTestDB :: IO Connection
connectToTestDB = connect defaultConnectInfo { 
    connectHost = "127.0.0.1", 
    connectUser = "user", 
    connectPassword = "password", 
    connectDatabase = "faculty_software" 
}

runInTransaction :: (Connection -> IO ()) -> IO ()
runInTransaction test = bracket connectToTestDB close $ \conn -> do
    void $ execute_ conn "BEGIN"
    test conn
    void $ execute_ conn "ROLLBACK"

main :: IO ()
main = hspec $ do
    
    describe "Database.Author" $ do
        around runInTransaction $ do

            it "can be inserted and then retrieved" $ \conn -> do
                let newAuthor = Author 0 "Test Author" "test@example.com"
                newId <- insert conn newAuthor
                maybeRetrievedAuthor <- findById @Author conn (fromIntegral newId)
                maybeRetrievedAuthor `shouldBe` Just (newAuthor { authorId = fromIntegral newId })

            it "can be updated" $ \conn -> do
                let originalAuthor = Author 0 "Original Name" "original@example.com"
                newId <- insert conn originalAuthor
                let updatedAuthor = (originalAuthor { authorId = fromIntegral newId, authorFullName = "Updated Name" })
                
                update conn updatedAuthor

                maybeRetrievedAuthor <- findById @Author conn (fromIntegral newId)
                fmap authorFullName maybeRetrievedAuthor `shouldBe` Just "Updated Name"

            it "can be deleted" $ \conn -> do
                let newAuthor = Author 0 "Deletable Author" "delete@me.com"
                newId <- insert conn newAuthor
                deleteById @Author conn (fromIntegral newId)
                maybeRetrievedAuthor <- findById @Author conn (fromIntegral newId)
                maybeRetrievedAuthor `shouldBe` Nothing

    describe "Database.User" $ do
        around runInTransaction $ do
            it "can be inserted and retrieved" $ \conn -> do
                let newUser = User 0 "testuser" "CS"
                newId <- insert conn newUser
                maybeUser <- findById @User conn (fromIntegral newId)
                maybeUser `shouldBe` Just (newUser { userId = fromIntegral newId })

    describe "Database.Relationships" $ do
        around runInTransaction $ do
            it "correctly links software to an author" $ \conn -> do
                let newLicense = License 0 "Test License" ""
                licId <- insert conn newLicense
                let newSoftware = Software 0 "TestApp" "" "" "" "" (fromIntegral licId)
                swId <- insert conn newSoftware
                let newAuthor = Author 0 "Test Author" ""
                authId <- insert conn newAuthor

                linkSoftwareToAuthor conn (fromIntegral swId) (fromIntegral authId)

                authors <- findAuthorsForSoftware conn (fromIntegral swId)
                length authors `shouldBe` 1
                authorId (head authors) `shouldBe` fromIntegral authId

    describe "Database.Statistics" $ do
        around runInTransaction $ do
            it "correctly records usage and reports popularity" $ \conn -> do
                let newLicense = License 0 "Test License" ""
                licId <- insert conn newLicense
                let softwareA = Software 0 "Software A" "" "" "" "" (fromIntegral licId)
                let softwareB = Software 0 "Software B" "" "" "" "" (fromIntegral licId)
                swIdA <- insert conn softwareA
                swIdB <- insert conn softwareB
                let newUser = User 0 "stat-user" ""
                usrId <- insert conn newUser

                replicateM_ 3 $ recordUsage conn (fromIntegral swIdA) (fromIntegral usrId)
                replicateM_ 2 $ recordUsage conn (fromIntegral swIdB) (fromIntegral usrId)

                report <- getPopularityReport conn

                length report `shouldBe` 2
                let mostPopular = head report
                let secondPopular = report !! 1

                popSwName mostPopular `shouldBe` "Software A"
                popUsageCount mostPopular `shouldBe` 3
                popSwName secondPopular `shouldBe` "Software B"
                popUsageCount secondPopular `shouldBe` 2