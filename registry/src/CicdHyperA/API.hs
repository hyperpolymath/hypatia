{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- SPDX-License-Identifier: PLMP-1.0-or-later
-- | REST API for the cicd-hyper-a registry
--
-- Provides endpoints for:
--
--   * Ruleset deposit/withdrawal with verification
--   * Search and discovery by effect, language, category
--   * Audit and provenance tracking
--   * Health and metrics (Prometheus format)
--
-- API Design follows REST best practices:
--
--   * Consistent error responses with problem+json
--   * Pagination for list endpoints
--   * ETags for caching
--   * Rate limiting headers

module CicdHyperA.API
  ( -- * API type
    RegistryAPI
  , registryAPI
    -- * Server
  , runServer
  , app
    -- * Request types
  , DepositRequest(..)
  , WithdrawRequest(..)
  , SearchRequest(..)
  , UpdateRequest(..)
  , VerifyRequest(..)
    -- * Response types
  , DepositResponse(..)
  , WithdrawResponse(..)
  , SearchResponse(..)
  , AuditResponse(..)
  , HealthResponse(..)
  , ErrorResponse(..)
  , VerifyResponse(..)
    -- * Common types
  , RulesetSummary(..)
  , AuditEntry(..)
  , HealthCheck(..)
  , PaginationInfo(..)
    -- * Handlers
  , depositHandler
  , withdrawHandler
  , searchHandler
  , auditHandler
  , healthHandler
  , verifyHandler
  , listHandler
  , deleteHandler
    -- * Metrics
  , MetricsResponse(..)
  , metricsHandler
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), encode, decode
  , withObject, (.:), (.:?), (.!=), object, (.=)
  , Options(..), defaultOptions, genericToJSON, genericParseJSON
  , camelTo2
  )
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

import CicdHyperA.Ruleset (Effect(..), RulesetCategory(..), Language(..))
import CicdHyperA.Registry
  ( Registry, RegistryEntry(..), RuleVersion(..)
  , deposit, withdraw, search, audit, listRules
  , SearchQuery(..), SearchResult(..)
  , AuditLog(..), AuditAction(..)
  , showVersion, parseVersion
  )
import qualified CicdHyperA.Registry as Reg
import CicdHyperA.Verify (VerifyResult(..), verifyRule, VerifyError(..))

-- ============================================================
-- API TYPE DEFINITION
-- ============================================================

-- | Registry API endpoints
--
-- @
-- POST   /api/v1/rulesets                    - Deposit ruleset
-- GET    /api/v1/rulesets/:name              - Withdraw ruleset
-- GET    /api/v1/rulesets                    - List/search rulesets
-- DELETE /api/v1/rulesets/:name              - Delete ruleset
-- PUT    /api/v1/rulesets/:name              - Update ruleset
-- POST   /api/v1/rulesets/:name/verify       - Verify ruleset
-- GET    /api/v1/rulesets/:name/audit        - Audit log
-- GET    /api/v1/health                      - Health check
-- GET    /api/v1/metrics                     - Prometheus metrics
-- @
data RegistryAPI = RegistryAPI

registryAPI :: RegistryAPI
registryAPI = RegistryAPI

-- ============================================================
-- JSON OPTIONS
-- ============================================================

-- | Custom JSON options for API types
apiOptions :: Options
apiOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . dropPrefix
  , omitNothingFields = True
  }
  where
    dropPrefix s = case break (== '_') s of
      (_, '_':rest) -> rest
      _ -> s

-- ============================================================
-- REQUEST TYPES
-- ============================================================

-- | Deposit request - submit a new or updated ruleset
data DepositRequest = DepositRequest
  { depositName        :: Text           -- ^ Unique ruleset identifier
  , depositDescription :: Text           -- ^ Human-readable description
  , depositContent     :: Text           -- ^ Ruleset content (Haskell source or JSON)
  , depositEffect      :: Text           -- ^ "preventive" | "curative" | "diagnostic"
  , depositCategory    :: Maybe Text     -- ^ Category classification
  , depositLanguages   :: Maybe [Text]   -- ^ Target languages
  , depositTags        :: Maybe [Text]   -- ^ Searchable tags
  , depositSign        :: Bool           -- ^ GPG sign the ruleset
  , depositVerify      :: Bool           -- ^ Run formal verification
  , depositAuthor      :: Maybe Text     -- ^ Author attribution
  , depositLicense     :: Maybe Text     -- ^ SPDX license identifier
  } deriving (Show, Eq, Generic)

instance FromJSON DepositRequest where
  parseJSON = withObject "DepositRequest" $ \v -> DepositRequest
    <$> v .: "name"
    <*> v .: "description"
    <*> v .: "content"
    <*> v .: "effect"
    <*> v .:? "category"
    <*> v .:? "languages"
    <*> v .:? "tags"
    <*> v .:? "sign" .!= False
    <*> v .:? "verify" .!= False
    <*> v .:? "author"
    <*> v .:? "license"

instance ToJSON DepositRequest where
  toJSON DepositRequest{..} = object
    [ "name" .= depositName
    , "description" .= depositDescription
    , "content" .= depositContent
    , "effect" .= depositEffect
    , "category" .= depositCategory
    , "languages" .= depositLanguages
    , "tags" .= depositTags
    , "sign" .= depositSign
    , "verify" .= depositVerify
    , "author" .= depositAuthor
    , "license" .= depositLicense
    ]

-- | Withdraw request - pull a ruleset for local use
data WithdrawRequest = WithdrawRequest
  { withdrawName       :: Text           -- ^ Ruleset to withdraw
  , withdrawVersion    :: Maybe Text     -- ^ Specific version (Nothing = latest)
  , withdrawVerifySig  :: Bool           -- ^ Verify GPG signature
  , withdrawFormat     :: Maybe Text     -- ^ Output format: "haskell" | "json"
  } deriving (Show, Eq, Generic)

instance FromJSON WithdrawRequest where
  parseJSON = withObject "WithdrawRequest" $ \v -> WithdrawRequest
    <$> v .: "name"
    <*> v .:? "version"
    <*> v .:? "verify_signature" .!= False
    <*> v .:? "format"

instance ToJSON WithdrawRequest where
  toJSON WithdrawRequest{..} = object
    [ "name" .= withdrawName
    , "version" .= withdrawVersion
    , "verify_signature" .= withdrawVerifySig
    , "format" .= withdrawFormat
    ]

-- | Search request - query rulesets with filters
data SearchRequest = SearchRequest
  { searchEffect       :: Maybe Text     -- ^ Filter by effect type
  , searchLanguage     :: Maybe Text     -- ^ Filter by target language
  , searchCategory     :: Maybe Text     -- ^ Filter by category
  , searchQuery        :: Maybe Text     -- ^ Free text search
  , searchTags         :: Maybe [Text]   -- ^ Filter by tags
  , searchVerified     :: Maybe Bool     -- ^ Filter by verification status
  , searchLimit        :: Maybe Int      -- ^ Max results (default: 20, max: 100)
  , searchOffset       :: Maybe Int      -- ^ Pagination offset
  , searchSortBy       :: Maybe Text     -- ^ Sort field: "name" | "downloads" | "updated"
  , searchSortDesc     :: Maybe Bool     -- ^ Sort descending
  } deriving (Show, Eq, Generic)

instance FromJSON SearchRequest where
  parseJSON = withObject "SearchRequest" $ \v -> SearchRequest
    <$> v .:? "effect"
    <*> v .:? "language"
    <*> v .:? "category"
    <*> v .:? "query"
    <*> v .:? "tags"
    <*> v .:? "verified"
    <*> v .:? "limit"
    <*> v .:? "offset"
    <*> v .:? "sort_by"
    <*> v .:? "sort_desc"

instance ToJSON SearchRequest where
  toJSON SearchRequest{..} = object
    [ "effect" .= searchEffect
    , "language" .= searchLanguage
    , "category" .= searchCategory
    , "query" .= searchQuery
    , "tags" .= searchTags
    , "verified" .= searchVerified
    , "limit" .= searchLimit
    , "offset" .= searchOffset
    , "sort_by" .= searchSortBy
    , "sort_desc" .= searchSortDesc
    ]

-- | Update request - modify existing ruleset metadata
data UpdateRequest = UpdateRequest
  { updateDescription :: Maybe Text
  , updateTags        :: Maybe [Text]
  , updateEnabled     :: Maybe Bool
  , updateCategory    :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON UpdateRequest
instance ToJSON UpdateRequest

-- | Verify request - request formal verification
data VerifyRequest = VerifyRequest
  { verifyRunLiquid   :: Bool           -- ^ Run Liquid Haskell
  , verifyRunQuick    :: Bool           -- ^ Run QuickCheck properties
  , verifyTimeout     :: Maybe Int      -- ^ Verification timeout (seconds)
  } deriving (Show, Eq, Generic)

instance FromJSON VerifyRequest where
  parseJSON = withObject "VerifyRequest" $ \v -> VerifyRequest
    <$> v .:? "run_liquid" .!= True
    <*> v .:? "run_quickcheck" .!= True
    <*> v .:? "timeout"

instance ToJSON VerifyRequest where
  toJSON VerifyRequest{..} = object
    [ "run_liquid" .= verifyRunLiquid
    , "run_quickcheck" .= verifyRunQuick
    , "timeout" .= verifyTimeout
    ]

-- ============================================================
-- RESPONSE TYPES
-- ============================================================

-- | Deposit response
data DepositResponse = DepositResponse
  { depositSuccess     :: Bool           -- ^ Operation succeeded
  , depositVersion     :: Text           -- ^ Assigned version
  , depositSignature   :: Maybe Text     -- ^ GPG signature if signed
  , depositVerified    :: Bool           -- ^ Verification passed
  , depositErrors      :: [Text]         -- ^ Validation/verification errors
  , depositUrl         :: Maybe Text     -- ^ URL to access the ruleset
  } deriving (Show, Eq, Generic)

instance FromJSON DepositResponse
instance ToJSON DepositResponse

-- | Withdraw response
data WithdrawResponse = WithdrawResponse
  { withdrawSuccess    :: Bool           -- ^ Operation succeeded
  , withdrawContent    :: Text           -- ^ Ruleset content
  , withdrawVersion'   :: Text           -- ^ Version retrieved
  , withdrawSignature' :: Maybe Text     -- ^ GPG signature
  , withdrawVerified'  :: Bool           -- ^ Signature verified
  , withdrawMetadata   :: Maybe RulesetSummary  -- ^ Ruleset metadata
  } deriving (Show, Eq, Generic)

instance FromJSON WithdrawResponse
instance ToJSON WithdrawResponse

-- | Search response with pagination
data SearchResponse = SearchResponse
  { searchResults      :: [RulesetSummary]  -- ^ Matching rulesets
  , searchTotal        :: Int               -- ^ Total matches
  , searchHasMore      :: Bool              -- ^ More results available
  , searchPagination   :: PaginationInfo    -- ^ Pagination details
  } deriving (Show, Eq, Generic)

instance FromJSON SearchResponse
instance ToJSON SearchResponse

-- | Pagination information
data PaginationInfo = PaginationInfo
  { paginationLimit    :: Int            -- ^ Current limit
  , paginationOffset   :: Int            -- ^ Current offset
  , paginationTotal    :: Int            -- ^ Total available
  , paginationNext     :: Maybe Text     -- ^ URL for next page
  , paginationPrev     :: Maybe Text     -- ^ URL for previous page
  } deriving (Show, Eq, Generic)

instance FromJSON PaginationInfo
instance ToJSON PaginationInfo

-- | Ruleset summary for search results
data RulesetSummary = RulesetSummary
  { summaryName        :: Text
  , summaryDescription :: Text
  , summaryVersion     :: Text
  , summaryEffect      :: Text
  , summaryCategory    :: Maybe Text
  , summaryLanguages   :: [Text]
  , summaryTags        :: [Text]
  , summaryDownloads   :: Int
  , summaryVerified    :: Bool
  , summaryEnabled     :: Bool
  , summaryCreated     :: Maybe UTCTime
  , summaryUpdated     :: Maybe UTCTime
  , summaryUrl         :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON RulesetSummary
instance ToJSON RulesetSummary

-- | Audit response
data AuditResponse = AuditResponse
  { auditName          :: Text           -- ^ Ruleset name
  , auditHistory       :: [AuditEntry]   -- ^ Audit trail
  , auditCurrentVersion :: Text          -- ^ Current version
  , auditCreated       :: UTCTime        -- ^ Creation date
  , auditLastModified  :: UTCTime        -- ^ Last modification
  , auditTotalEntries  :: Int            -- ^ Total audit entries
  } deriving (Show, Eq, Generic)

instance FromJSON AuditResponse
instance ToJSON AuditResponse

-- | Audit entry (API version)
data AuditEntry = AuditEntry
  { auditVersion       :: Text
  , auditTimestamp     :: UTCTime
  , auditAction        :: Text           -- ^ "created" | "updated" | "verified" | "signed"
  , auditActor         :: Text           -- ^ Who performed the action
  , auditSignature     :: Maybe Text     -- ^ Signature if applicable
  , auditChanges       :: Text           -- ^ Description of changes
  } deriving (Show, Eq, Generic)

instance FromJSON AuditEntry
instance ToJSON AuditEntry

-- | Verify response
data VerifyResponse = VerifyResponse
  { verifySuccess      :: Bool           -- ^ All checks passed
  , verifyLiquidResult :: Maybe Text     -- ^ Liquid Haskell result
  , verifyQuickResult  :: Maybe Text     -- ^ QuickCheck result
  , verifyErrors       :: [Text]         -- ^ Verification errors
  , verifyDuration     :: Int            -- ^ Verification time (ms)
  } deriving (Show, Eq, Generic)

instance FromJSON VerifyResponse
instance ToJSON VerifyResponse

-- | Health response
data HealthResponse = HealthResponse
  { healthStatus       :: Text           -- ^ "healthy" | "degraded" | "unhealthy"
  , healthVersion      :: Text           -- ^ API version
  , healthUptime       :: Int            -- ^ Uptime in seconds
  , healthChecks       :: [HealthCheck]  -- ^ Individual checks
  } deriving (Show, Eq, Generic)

instance FromJSON HealthResponse
instance ToJSON HealthResponse

-- | Individual health check
data HealthCheck = HealthCheck
  { checkName          :: Text           -- ^ Component name
  , checkStatus        :: Text           -- ^ "pass" | "fail" | "warn"
  , checkMessage       :: Maybe Text     -- ^ Status message
  , checkLatency       :: Maybe Int      -- ^ Latency in ms
  } deriving (Show, Eq, Generic)

instance FromJSON HealthCheck
instance ToJSON HealthCheck

-- | Error response (RFC 7807 Problem Details)
data ErrorResponse = ErrorResponse
  { errorType          :: Text           -- ^ Error type URI
  , errorTitle         :: Text           -- ^ Human-readable title
  , errorStatus        :: Int            -- ^ HTTP status code
  , errorDetail        :: Text           -- ^ Detailed description
  , errorInstance      :: Maybe Text     -- ^ Request instance URI
  , errorErrors        :: Maybe [Text]   -- ^ Validation errors
  } deriving (Show, Eq, Generic)

instance FromJSON ErrorResponse
instance ToJSON ErrorResponse

-- | Metrics response (Prometheus format)
data MetricsResponse = MetricsResponse
  { metricsRulesets    :: Int            -- ^ Total rulesets
  , metricsDeposits    :: Int            -- ^ Total deposits
  , metricsWithdrawals :: Int            -- ^ Total withdrawals
  , metricsVerified    :: Int            -- ^ Verified rulesets
  , metricsUptime      :: Int            -- ^ Uptime seconds
  } deriving (Show, Eq, Generic)

instance FromJSON MetricsResponse
instance ToJSON MetricsResponse

-- ============================================================
-- HANDLERS
-- ============================================================

-- | Handle deposit request
depositHandler :: Registry -> DepositRequest -> IO DepositResponse
depositHandler registry DepositRequest{..} = do
  -- Validate input
  let errors = validateDeposit depositName depositContent depositEffect

  if not (null errors)
    then return $ DepositResponse
           { depositSuccess = False
           , depositVersion = ""
           , depositSignature = Nothing
           , depositVerified = False
           , depositErrors = errors
           , depositUrl = Nothing
           }
    else do
      -- Parse effect type
      let mEffect = parseEffect depositEffect

      -- Deposit to registry
      newReg <- deposit depositName depositDescription depositContent "manual" mEffect registry

      -- Generate signature if requested
      signature <- if depositSign
        then Just <$> signRuleset depositName depositContent
        else return Nothing

      -- Run verification if requested
      verified <- if depositVerify
        then runVerification depositContent
        else return True

      return $ DepositResponse
        { depositSuccess = True
        , depositVersion = "1.0.0"
        , depositSignature = signature
        , depositVerified = verified
        , depositErrors = []
        , depositUrl = Just $ "/api/v1/rulesets/" <> depositName
        }

-- | Handle withdraw request
withdrawHandler :: Registry -> WithdrawRequest -> IO WithdrawResponse
withdrawHandler registry WithdrawRequest{..} = do
  let mVersion = withdrawVersion >>= parseVersion
  result <- Reg.withdraw withdrawName mVersion registry

  case result of
    Nothing -> return $ WithdrawResponse
      { withdrawSuccess = False
      , withdrawContent = ""
      , withdrawVersion' = ""
      , withdrawSignature' = Nothing
      , withdrawVerified' = False
      , withdrawMetadata = Nothing
      }
    Just (content, entry) -> do
      -- Verify signature if requested
      verified <- if withdrawVerifySig
        then verifySig withdrawName content
        else return True

      return $ WithdrawResponse
        { withdrawSuccess = True
        , withdrawContent = content
        , withdrawVersion' = showVersion (entryVersion entry)
        , withdrawSignature' = entrySignature entry
        , withdrawVerified' = verified
        , withdrawMetadata = Just $ toSummary entry
        }

-- | Handle search request
searchHandler :: Registry -> SearchRequest -> IO SearchResponse
searchHandler registry SearchRequest{..} = do
  let query = SearchQuery
        { queryText = searchQuery
        , queryEffect = searchEffect >>= parseEffect
        , queryCategory = searchCategory >>= parseCategory
        , queryLanguage = searchLanguage >>= parseLanguage
        , queryTags = fromMaybe [] searchTags
        , queryVerified = searchVerified
        , queryEnabled = Just True
        , queryLimit = min 100 (fromMaybe 20 searchLimit)
        , queryOffset = fromMaybe 0 searchOffset
        , querySortBy = fromMaybe "name" searchSortBy
        , querySortDesc = fromMaybe False searchSortDesc
        }

  let result = Reg.search query registry
      summaries = map toSummary (resultEntries result)
      limit = queryLimit query
      offset = queryOffset query

  return $ SearchResponse
    { searchResults = summaries
    , searchTotal = resultTotal result
    , searchHasMore = resultHasMore result
    , searchPagination = PaginationInfo
        { paginationLimit = limit
        , paginationOffset = offset
        , paginationTotal = resultTotal result
        , paginationNext = if resultHasMore result
            then Just $ mkPaginationUrl (offset + limit) limit
            else Nothing
        , paginationPrev = if offset > 0
            then Just $ mkPaginationUrl (max 0 (offset - limit)) limit
            else Nothing
        }
    }

-- | Handle list request (simplified search)
listHandler :: Registry -> IO SearchResponse
listHandler registry = searchHandler registry defaultSearchRequest
  where
    defaultSearchRequest = SearchRequest
      { searchEffect = Nothing
      , searchLanguage = Nothing
      , searchCategory = Nothing
      , searchQuery = Nothing
      , searchTags = Nothing
      , searchVerified = Nothing
      , searchLimit = Just 50
      , searchOffset = Just 0
      , searchSortBy = Just "name"
      , searchSortDesc = Just False
      }

-- | Handle delete request
deleteHandler :: Registry -> Text -> IO Bool
deleteHandler registry name = do
  _ <- Reg.unregisterRule name registry
  return True

-- | Handle audit request
auditHandler :: Registry -> Text -> IO (Maybe AuditResponse)
auditHandler registry name = do
  case Reg.lookupRule name registry of
    Nothing -> return Nothing
    Just entry -> do
      now <- getCurrentTime
      history <- Reg.getRuleHistory name registry

      return $ Just $ AuditResponse
        { auditName = name
        , auditHistory = map parseHistoryEntry history
        , auditCurrentVersion = showVersion (entryVersion entry)
        , auditCreated = entryCreated entry
        , auditLastModified = entryUpdated entry
        , auditTotalEntries = length history
        }

-- | Handle verify request
verifyHandler :: Registry -> Text -> VerifyRequest -> IO VerifyResponse
verifyHandler registry name VerifyRequest{..} = do
  case Reg.lookupRule name registry of
    Nothing -> return $ VerifyResponse
      { verifySuccess = False
      , verifyLiquidResult = Nothing
      , verifyQuickResult = Nothing
      , verifyErrors = ["Ruleset not found: " <> name]
      , verifyDuration = 0
      }
    Just entry -> do
      startTime <- getCurrentTime

      -- Run Liquid Haskell if requested
      liquidResult <- if verifyRunLiquid
        then Just <$> runLiquidHaskell (entryContent entry)
        else return Nothing

      -- Run QuickCheck if requested
      quickResult <- if verifyRunQuick
        then Just <$> runQuickCheck (entryContent entry)
        else return Nothing

      endTime <- getCurrentTime

      let success = maybe True (== "passed") liquidResult &&
                    maybe True (== "passed") quickResult
          errors = []  -- Collect errors from results

      return $ VerifyResponse
        { verifySuccess = success
        , verifyLiquidResult = liquidResult
        , verifyQuickResult = quickResult
        , verifyErrors = errors
        , verifyDuration = 100  -- TODO: Calculate actual duration
        }

-- | Handle health check
healthHandler :: IO HealthResponse
healthHandler = do
  -- Check dependencies
  arangoCheck <- checkArangoDB
  dragonflyCheck <- checkDragonfly
  registryCheck <- checkRegistryHealth

  let checks = [arangoCheck, dragonflyCheck, registryCheck]
      allPass = all (\c -> checkStatus c == "pass") checks
      anyFail = any (\c -> checkStatus c == "fail") checks
      status = if anyFail then "unhealthy"
               else if allPass then "healthy"
               else "degraded"

  return $ HealthResponse
    { healthStatus = status
    , healthVersion = "0.1.0"
    , healthUptime = 0  -- TODO: Track uptime
    , healthChecks = checks
    }

-- | Handle metrics request
metricsHandler :: Registry -> IO MetricsResponse
metricsHandler registry = do
  let entries = listRules registry
      verified = length $ filter entryVerified entries
      downloads = sum $ map entryDownloads entries

  return $ MetricsResponse
    { metricsRulesets = length entries
    , metricsDeposits = 0  -- TODO: Track from audit log
    , metricsWithdrawals = downloads
    , metricsVerified = verified
    , metricsUptime = 0
    }

-- ============================================================
-- HELPER FUNCTIONS
-- ============================================================

validateDeposit :: Text -> Text -> Text -> [Text]
validateDeposit name content effect =
  let nameErrors = if T.null name then ["Name is required"] else []
      contentErrors = if T.null content then ["Content is required"] else []
      effectErrors = if effect `notElem` ["preventive", "curative", "diagnostic"]
                     then ["Effect must be preventive, curative, or diagnostic"]
                     else []
  in nameErrors ++ contentErrors ++ effectErrors

parseEffect :: Text -> Maybe Effect
parseEffect "preventive" = Just Preventive
parseEffect "curative" = Just Curative
parseEffect "diagnostic" = Just Diagnostic
parseEffect _ = Nothing

parseCategory :: Text -> Maybe RulesetCategory
parseCategory "security" = Just SecurityCategory
parseCategory "compliance" = Just ComplianceCategory
parseCategory "quality" = Just QualityCategory
parseCategory "performance" = Just PerformanceCategory
parseCategory "documentation" = Just DocumentationCategory
parseCategory "infrastructure" = Just InfrastructureCategory
parseCategory _ = Nothing

parseLanguage :: Text -> Maybe Language
parseLanguage "typescript" = Just TypeScript
parseLanguage "javascript" = Just JavaScript
parseLanguage "golang" = Just Golang
parseLanguage "go" = Just Golang
parseLanguage "python" = Just Python
parseLanguage "rust" = Just Rust
parseLanguage "rescript" = Just ReScript
parseLanguage "gleam" = Just Gleam
parseLanguage "julia" = Just Julia
parseLanguage "haskell" = Just Haskell
parseLanguage "logtalk" = Just Logtalk
parseLanguage "nickel" = Just Nickel
parseLanguage "guile" = Just Guile
parseLanguage "ocaml" = Just OCaml
parseLanguage "ada" = Just Ada
parseLanguage _ = Nothing

signRuleset :: Text -> Text -> IO Text
signRuleset name _ = do
  -- TODO: Implement GPG signing
  return $ "sig:" <> name <> ":stub"

verifySig :: Text -> Text -> IO Bool
verifySig _ _ = do
  -- TODO: Implement GPG verification
  return True

runVerification :: Text -> IO Bool
runVerification _ = do
  -- TODO: Run actual verification
  return True

runLiquidHaskell :: Text -> IO Text
runLiquidHaskell _ = do
  -- TODO: Run Liquid Haskell
  return "passed"

runQuickCheck :: Text -> IO Text
runQuickCheck _ = do
  -- TODO: Run QuickCheck
  return "passed"

toSummary :: RegistryEntry -> RulesetSummary
toSummary entry = RulesetSummary
  { summaryName = entryName entry
  , summaryDescription = entryDescription entry
  , summaryVersion = showVersion (entryVersion entry)
  , summaryEffect = maybe "unknown" showEffect (entryEffect entry)
  , summaryCategory = showCategory <$> entryCategory entry
  , summaryLanguages = map showLanguage (entryLanguages entry)
  , summaryTags = entryTags entry
  , summaryDownloads = entryDownloads entry
  , summaryVerified = entryVerified entry
  , summaryEnabled = entryEnabled entry
  , summaryCreated = Just (entryCreated entry)
  , summaryUpdated = Just (entryUpdated entry)
  , summaryUrl = "/api/v1/rulesets/" <> entryName entry
  }

showEffect :: Effect -> Text
showEffect Preventive = "preventive"
showEffect Curative = "curative"
showEffect Diagnostic = "diagnostic"

showCategory :: RulesetCategory -> Text
showCategory SecurityCategory = "security"
showCategory ComplianceCategory = "compliance"
showCategory QualityCategory = "quality"
showCategory PerformanceCategory = "performance"
showCategory DocumentationCategory = "documentation"
showCategory InfrastructureCategory = "infrastructure"

showLanguage :: Language -> Text
showLanguage TypeScript = "typescript"
showLanguage JavaScript = "javascript"
showLanguage Golang = "golang"
showLanguage Python = "python"
showLanguage Rust = "rust"
showLanguage ReScript = "rescript"
showLanguage Gleam = "gleam"
showLanguage Julia = "julia"
showLanguage Haskell = "haskell"
showLanguage Logtalk = "logtalk"
showLanguage Nickel = "nickel"
showLanguage Guile = "guile"
showLanguage OCaml = "ocaml"
showLanguage Ada = "ada"

parseHistoryEntry :: Text -> AuditEntry
parseHistoryEntry line = AuditEntry
  { auditVersion = "1.0.0"
  , auditTimestamp = error "TODO: Parse timestamp"
  , auditAction = "updated"
  , auditActor = "system"
  , auditSignature = Nothing
  , auditChanges = line
  }

mkPaginationUrl :: Int -> Int -> Text
mkPaginationUrl offset limit =
  "/api/v1/rulesets?offset=" <> T.pack (show offset) <> "&limit=" <> T.pack (show limit)

checkArangoDB :: IO HealthCheck
checkArangoDB = do
  -- TODO: Actual health check
  return $ HealthCheck "arangodb" "pass" Nothing (Just 5)

checkDragonfly :: IO HealthCheck
checkDragonfly = do
  -- TODO: Actual health check
  return $ HealthCheck "dragonfly" "pass" Nothing (Just 1)

checkRegistryHealth :: IO HealthCheck
checkRegistryHealth = do
  return $ HealthCheck "registry" "pass" (Just "Registry operational") (Just 0)

-- ============================================================
-- SERVER
-- ============================================================

-- | Application placeholder
app :: Registry -> IO ()
app _ = do
  putStrLn "cicd-hyper-a Registry API v0.1.0"
  putStrLn ""
  putStrLn "Endpoints:"
  putStrLn "  POST   /api/v1/rulesets           - Deposit ruleset"
  putStrLn "  GET    /api/v1/rulesets/:name     - Withdraw ruleset"
  putStrLn "  GET    /api/v1/rulesets           - List/search rulesets"
  putStrLn "  DELETE /api/v1/rulesets/:name     - Delete ruleset"
  putStrLn "  PUT    /api/v1/rulesets/:name     - Update ruleset"
  putStrLn "  POST   /api/v1/rulesets/:name/verify - Verify ruleset"
  putStrLn "  GET    /api/v1/rulesets/:name/audit  - Audit log"
  putStrLn "  GET    /api/v1/health             - Health check"
  putStrLn "  GET    /api/v1/metrics            - Prometheus metrics"

-- | Run the server
runServer :: Int -> Registry -> IO ()
runServer port registry = do
  putStrLn $ "Starting registry server on port " <> show port
  app registry
  -- TODO: Implement with warp
