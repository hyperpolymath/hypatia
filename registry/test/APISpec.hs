{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: AGPL-3.0-or-later
-- | Tests for API module

module APISpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import CicdHyperA.API
import CicdHyperA.Registry (initRegistry)
import System.IO.Temp (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "Request serialization" $ do
    it "serializes DepositRequest" $ do
      let req = DepositRequest
            { depositName = "test-rule"
            , depositDescription = "Test description"
            , depositContent = "-- Haskell code"
            , depositEffect = "preventive"
            , depositCategory = Just "security"
            , depositLanguages = Just ["rust", "haskell"]
            , depositTags = Just ["security", "openssf"]
            , depositSign = True
            , depositVerify = True
            , depositAuthor = Just "hyperpolymath"
            , depositLicense = Just "AGPL-3.0-or-later"
            }
      (decode . encode $ req) `shouldBe` Just req

    it "serializes WithdrawRequest" $ do
      let req = WithdrawRequest
            { withdrawName = "my-ruleset"
            , withdrawVersion = Just "1.2.0"
            , withdrawVerifySig = True
            , withdrawFormat = Just "haskell"
            }
      (decode . encode $ req) `shouldBe` Just req

    it "serializes SearchRequest" $ do
      let req = SearchRequest
            { searchEffect = Just "curative"
            , searchLanguage = Just "rust"
            , searchCategory = Just "security"
            , searchQuery = Just "openssf"
            , searchTags = Just ["security"]
            , searchVerified = Just True
            , searchLimit = Just 10
            , searchOffset = Just 5
            , searchSortBy = Just "downloads"
            , searchSortDesc = Just True
            }
      (decode . encode $ req) `shouldBe` Just req

    it "serializes VerifyRequest" $ do
      let req = VerifyRequest
            { verifyRunLiquid = True
            , verifyRunQuick = True
            , verifyTimeout = Just 300
            }
      (decode . encode $ req) `shouldBe` Just req

  describe "Response serialization" $ do
    it "serializes DepositResponse" $ do
      let resp = DepositResponse
            { depositSuccess = True
            , depositVersion = "1.0.0"
            , depositSignature = Just "sig:test"
            , depositVerified = True
            , depositErrors = []
            , depositUrl = Just "/api/v1/rulesets/test"
            }
      (decode . encode $ resp) `shouldBe` Just resp

    it "serializes WithdrawResponse" $ do
      let resp = WithdrawResponse
            { withdrawSuccess = True
            , withdrawContent = "-- content"
            , withdrawVersion' = "1.0.0"
            , withdrawSignature' = Nothing
            , withdrawVerified' = True
            , withdrawMetadata = Nothing
            }
      (decode . encode $ resp) `shouldBe` Just resp

    it "serializes HealthResponse" $ do
      let resp = HealthResponse
            { healthStatus = "healthy"
            , healthVersion = "0.1.0"
            , healthUptime = 3600
            , healthChecks =
                [ HealthCheck "arangodb" "pass" Nothing (Just 5)
                , HealthCheck "dragonfly" "pass" Nothing (Just 1)
                ]
            }
      (decode . encode $ resp) `shouldBe` Just resp

    it "serializes VerifyResponse" $ do
      let resp = VerifyResponse
            { verifySuccess = True
            , verifyLiquidResult = Just "passed"
            , verifyQuickResult = Just "passed"
            , verifyErrors = []
            , verifyDuration = 150
            }
      (decode . encode $ resp) `shouldBe` Just resp

    it "serializes ErrorResponse" $ do
      let resp = ErrorResponse
            { errorType = "urn:cicd-hyper-a:validation-error"
            , errorTitle = "Validation Error"
            , errorStatus = 400
            , errorDetail = "Name is required"
            , errorInstance = Just "/api/v1/rulesets"
            , errorErrors = Just ["Name is required", "Content is required"]
            }
      (decode . encode $ resp) `shouldBe` Just resp

    it "serializes MetricsResponse" $ do
      let resp = MetricsResponse
            { metricsRulesets = 42
            , metricsDeposits = 100
            , metricsWithdrawals = 500
            , metricsVerified = 35
            , metricsUptime = 86400
            }
      (decode . encode $ resp) `shouldBe` Just resp

    it "serializes PaginationInfo" $ do
      let info = PaginationInfo
            { paginationLimit = 20
            , paginationOffset = 40
            , paginationTotal = 100
            , paginationNext = Just "/api/v1/rulesets?offset=60&limit=20"
            , paginationPrev = Just "/api/v1/rulesets?offset=20&limit=20"
            }
      (decode . encode $ info) `shouldBe` Just info

  describe "Deposit handler" $ do
    it "validates empty name" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = DepositRequest "" "desc" "content" "preventive" Nothing Nothing Nothing False False Nothing Nothing
        resp <- depositHandler reg req
        depositSuccess resp `shouldBe` False
        depositErrors resp `shouldContain` ["Name is required"]

    it "validates empty content" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = DepositRequest "test" "desc" "" "preventive" Nothing Nothing Nothing False False Nothing Nothing
        resp <- depositHandler reg req
        depositSuccess resp `shouldBe` False
        depositErrors resp `shouldContain` ["Content is required"]

    it "validates effect type" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = DepositRequest "test" "desc" "content" "invalid" Nothing Nothing Nothing False False Nothing Nothing
        resp <- depositHandler reg req
        depositSuccess resp `shouldBe` False
        depositErrors resp `shouldSatisfy` any ("Effect" `elem`)

    it "accepts valid deposit" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = DepositRequest "test-rule" "desc" "content" "preventive" Nothing Nothing Nothing False False Nothing Nothing
        resp <- depositHandler reg req
        depositSuccess resp `shouldBe` True
        depositVersion resp `shouldBe` "1.0.0"
        depositUrl resp `shouldBe` Just "/api/v1/rulesets/test-rule"

    it "accepts curative effect" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = DepositRequest "test-rule" "desc" "content" "curative" Nothing Nothing Nothing False False Nothing Nothing
        resp <- depositHandler reg req
        depositSuccess resp `shouldBe` True

    it "accepts diagnostic effect" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = DepositRequest "test-rule" "desc" "content" "diagnostic" Nothing Nothing Nothing False False Nothing Nothing
        resp <- depositHandler reg req
        depositSuccess resp `shouldBe` True

  describe "Withdraw handler" $ do
    it "returns failure for unknown ruleset" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = WithdrawRequest "nonexistent" Nothing False Nothing
        resp <- withdrawHandler reg req
        withdrawSuccess resp `shouldBe` False

  describe "Search handler" $ do
    it "returns empty for empty registry" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = SearchRequest Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        resp <- searchHandler reg req
        searchTotal resp `shouldBe` 0
        searchResults resp `shouldBe` []

    it "respects limit parameter" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = SearchRequest Nothing Nothing Nothing Nothing Nothing Nothing (Just 5) Nothing Nothing Nothing
        resp <- searchHandler reg req
        length (searchResults resp) `shouldSatisfy` (<= 5)

    it "provides pagination info" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = SearchRequest Nothing Nothing Nothing Nothing Nothing Nothing (Just 10) (Just 0) Nothing Nothing
        resp <- searchHandler reg req
        paginationLimit (searchPagination resp) `shouldBe` 10
        paginationOffset (searchPagination resp) `shouldBe` 0

  describe "Health handler" $ do
    it "returns healthy status" $ do
      resp <- healthHandler
      healthStatus resp `shouldBe` "healthy"
      healthVersion resp `shouldBe` "0.1.0"

    it "includes dependency checks" $ do
      resp <- healthHandler
      length (healthChecks resp) `shouldSatisfy` (>= 2)
      map checkName (healthChecks resp) `shouldContain` ["arangodb", "dragonfly"]

    it "all checks pass" $ do
      resp <- healthHandler
      all ((== "pass") . checkStatus) (healthChecks resp) `shouldBe` True

  describe "Verify handler" $ do
    it "returns failure for unknown ruleset" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = VerifyRequest True True Nothing
        resp <- verifyHandler reg "nonexistent" req
        verifySuccess resp `shouldBe` False
        verifyErrors resp `shouldSatisfy` (not . null)

  describe "List handler" $ do
    it "returns empty for empty registry" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        resp <- listHandler reg
        searchTotal resp `shouldBe` 0

  describe "Metrics handler" $ do
    it "returns zero metrics for empty registry" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        resp <- metricsHandler reg
        metricsRulesets resp `shouldBe` 0
        metricsVerified resp `shouldBe` 0
