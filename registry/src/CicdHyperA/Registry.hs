{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
-- SPDX-License-Identifier: PLMP-1.0-or-later
-- | Git-based rule registry with versioning
--
-- This module provides a persistent registry for storing and retrieving
-- verified rulesets. Key features:
--
--   * Git-backed storage for full version history
--   * GPG signature support for provenance
--   * Search and filtering by effect, language, category
--   * Audit trail for all operations
--
-- The registry follows a deposit/withdraw model similar to a package registry.

module CicdHyperA.Registry
  ( -- * Registry types
    Registry(..)
  , RegistryEntry(..)
  , RuleVersion(..)
  , RegistryConfig(..)
    -- * Audit types
  , AuditLog(..)
  , AuditAction(..)
  , AuditEntry(..)
    -- * Search types
  , SearchQuery(..)
  , SearchResult(..)
    -- * Registry operations
  , initRegistry
  , loadRegistry
  , saveRegistry
  , defaultConfig
    -- * Rule management
  , registerRule
  , unregisterRule
  , lookupRule
  , listRules
  , updateRule
    -- * Version management
  , getVersion
  , setVersion
  , bumpVersion
  , getRuleHistory
    -- * Deposit/Withdraw operations
  , deposit
  , withdraw
  , withdrawLatest
  , withdrawVersion
    -- * Search operations
  , search
  , searchByEffect
  , searchByLanguage
  , searchByCategory
  , searchByTags
    -- * Audit operations
  , audit
  , getAuditLog
  , recordAudit
    -- * Version helpers
  , showVersion
  , parseVersion
  , compareVersions
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import Data.List (sortBy, find)
import Data.Ord (comparing, Down(..))
import Data.Maybe (mapMaybe, fromMaybe)
import System.Directory (doesFileExist, createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>), takeExtension)
import Control.Monad (when, forM, forM_)
import GHC.Generics (Generic)

import CicdHyperA.Ruleset (Effect(..), RulesetCategory(..), Language(..))

-- ============================================================
-- Core Types
-- ============================================================

-- | Rule version (semantic versioning)
data RuleVersion = RuleVersion
  { vMajor :: Int
  , vMinor :: Int
  , vPatch :: Int
  } deriving (Show, Eq, Ord, Generic)

-- | Registry entry for a single ruleset
data RegistryEntry = RegistryEntry
  { entryName         :: Text           -- ^ Unique ruleset identifier
  , entryVersion      :: RuleVersion    -- ^ Current version
  , entryDescription  :: Text           -- ^ Human-readable description
  , entryCreated      :: UTCTime        -- ^ Creation timestamp
  , entryUpdated      :: UTCTime        -- ^ Last update timestamp
  , entryEnabled      :: Bool           -- ^ Whether ruleset is active
  , entrySource       :: Text           -- ^ "learned" | "manual" | "imported"
  , entryEffect       :: Maybe Effect   -- ^ Primary effect type
  , entryCategory     :: Maybe RulesetCategory  -- ^ Category
  , entryLanguages    :: [Language]     -- ^ Target languages
  , entryTags         :: [Text]         -- ^ Searchable tags
  , entrySignature    :: Maybe Text     -- ^ GPG signature
  , entryVerified     :: Bool           -- ^ Formally verified
  , entryDownloads    :: Int            -- ^ Download count
  , entryContent      :: Text           -- ^ Serialized ruleset content
  } deriving (Show, Eq, Generic)

-- | Registry configuration
data RegistryConfig = RegistryConfig
  { configStoragePath   :: FilePath     -- ^ Base path for storage
  , configGpgKeyId      :: Maybe Text   -- ^ GPG key for signing
  , configVerifyOnDeposit :: Bool       -- ^ Run verification on deposit
  , configRequireSignature :: Bool      -- ^ Require signatures
  , configMaxVersions   :: Int          -- ^ Max versions to keep per ruleset
  , configAuditEnabled  :: Bool         -- ^ Enable audit logging
  } deriving (Show, Eq, Generic)

-- | Rule registry
data Registry = Registry
  { regPath    :: FilePath              -- ^ Storage path
  , regConfig  :: RegistryConfig        -- ^ Configuration
  , regEntries :: Map Text RegistryEntry  -- ^ Registered rulesets
  , regVersion :: RuleVersion           -- ^ Registry schema version
  , regAudit   :: [AuditEntry]          -- ^ Audit log (recent entries)
  } deriving (Show)

-- ============================================================
-- Audit Types
-- ============================================================

-- | Audit action types
data AuditAction
  = AuditDeposit        -- ^ Ruleset deposited
  | AuditWithdraw       -- ^ Ruleset withdrawn
  | AuditUpdate         -- ^ Ruleset updated
  | AuditDelete         -- ^ Ruleset deleted
  | AuditVerify         -- ^ Ruleset verified
  | AuditSign           -- ^ Ruleset signed
  | AuditEnable         -- ^ Ruleset enabled
  | AuditDisable        -- ^ Ruleset disabled
  deriving (Show, Eq, Generic)

-- | Audit log entry
data AuditEntry = AuditEntry
  { auditTimestamp :: UTCTime           -- ^ When action occurred
  , auditAction    :: AuditAction       -- ^ What action was taken
  , auditRuleset   :: Text              -- ^ Affected ruleset
  , auditVersion   :: Maybe RuleVersion -- ^ Version involved
  , auditActor     :: Text              -- ^ Who performed the action
  , auditDetails   :: Text              -- ^ Additional details
  , auditSignature :: Maybe Text        -- ^ Signature if applicable
  } deriving (Show, Eq, Generic)

-- | Complete audit log for a ruleset
data AuditLog = AuditLog
  { logRuleset     :: Text              -- ^ Ruleset name
  , logEntries     :: [AuditEntry]      -- ^ All audit entries
  , logCreated     :: UTCTime           -- ^ First entry timestamp
  , logLastUpdated :: UTCTime           -- ^ Last entry timestamp
  } deriving (Show, Eq, Generic)

-- ============================================================
-- Search Types
-- ============================================================

-- | Search query parameters
data SearchQuery = SearchQuery
  { queryText     :: Maybe Text         -- ^ Free text search
  , queryEffect   :: Maybe Effect       -- ^ Filter by effect
  , queryCategory :: Maybe RulesetCategory  -- ^ Filter by category
  , queryLanguage :: Maybe Language     -- ^ Filter by language
  , queryTags     :: [Text]             -- ^ Filter by tags
  , queryVerified :: Maybe Bool         -- ^ Filter by verification status
  , queryEnabled  :: Maybe Bool         -- ^ Filter by enabled status
  , queryLimit    :: Int                -- ^ Max results
  , queryOffset   :: Int                -- ^ Pagination offset
  , querySortBy   :: Text               -- ^ Sort field
  , querySortDesc :: Bool               -- ^ Sort descending
  } deriving (Show, Eq, Generic)

-- | Search result container
data SearchResult = SearchResult
  { resultEntries  :: [RegistryEntry]   -- ^ Matching entries
  , resultTotal    :: Int               -- ^ Total matches (before pagination)
  , resultHasMore  :: Bool              -- ^ More results available
  , resultQuery    :: SearchQuery       -- ^ Original query
  } deriving (Show, Eq, Generic)

-- ============================================================
-- Default Configuration
-- ============================================================

-- | Default registry configuration
defaultConfig :: RegistryConfig
defaultConfig = RegistryConfig
  { configStoragePath = ".cicd-hyper-a"
  , configGpgKeyId = Nothing
  , configVerifyOnDeposit = True
  , configRequireSignature = False
  , configMaxVersions = 10
  , configAuditEnabled = True
  }

-- ============================================================
-- Registry Operations
-- ============================================================

-- | Initialize a new registry
initRegistry :: FilePath -> IO Registry
initRegistry path = do
  createDirectoryIfMissing True path
  createDirectoryIfMissing True (path </> "rulesets")
  createDirectoryIfMissing True (path </> "audit")
  createDirectoryIfMissing True (path </> "signatures")
  now <- getCurrentTime
  let reg = Registry
        { regPath = path
        , regConfig = defaultConfig { configStoragePath = path }
        , regEntries = Map.empty
        , regVersion = RuleVersion 1 0 0
        , regAudit = []
        }
  saveRegistry reg
  return reg

-- | Load registry from disk
loadRegistry :: FilePath -> IO (Maybe Registry)
loadRegistry path = do
  let indexFile = path </> "index.rules"
  exists <- doesFileExist indexFile
  if exists
    then do
      content <- TIO.readFile indexFile
      entries <- loadEntries (path </> "rulesets")
      auditLog <- loadAuditLog (path </> "audit")
      return $ Just Registry
        { regPath = path
        , regConfig = defaultConfig { configStoragePath = path }
        , regEntries = entries
        , regVersion = RuleVersion 1 0 0
        , regAudit = auditLog
        }
    else return Nothing

-- | Save registry to disk
saveRegistry :: Registry -> IO ()
saveRegistry Registry{..} = do
  let indexFile = regPath </> "index.rules"
  createDirectoryIfMissing True regPath
  TIO.writeFile indexFile (serializeRegistry regEntries regVersion)
  -- Save individual entries
  forM_ (Map.elems regEntries) $ \entry -> do
    let entryPath = regPath </> "rulesets" </> T.unpack (entryName entry) <> ".ruleset"
    TIO.writeFile entryPath (serializeEntry entry)

-- ============================================================
-- Deposit/Withdraw Operations
-- ============================================================

-- | Deposit a new or updated ruleset
deposit :: Text -> Text -> Text -> Text -> Maybe Effect -> Registry -> IO Registry
deposit name desc content source mEffect reg = do
  now <- getCurrentTime
  let existingEntry = Map.lookup name (regEntries reg)
      newVersion = case existingEntry of
        Nothing -> RuleVersion 1 0 0
        Just e  -> bumpPatch (entryVersion e)
      entry = RegistryEntry
        { entryName = name
        , entryVersion = newVersion
        , entryDescription = desc
        , entryCreated = maybe now entryCreated existingEntry
        , entryUpdated = now
        , entryEnabled = True
        , entrySource = source
        , entryEffect = mEffect
        , entryCategory = Nothing
        , entryLanguages = []
        , entryTags = []
        , entrySignature = Nothing
        , entryVerified = False
        , entryDownloads = maybe 0 entryDownloads existingEntry
        , entryContent = content
        }
      newEntries = Map.insert name entry (regEntries reg)
      auditEntry = AuditEntry
        { auditTimestamp = now
        , auditAction = maybe AuditDeposit (const AuditUpdate) existingEntry
        , auditRuleset = name
        , auditVersion = Just newVersion
        , auditActor = "system"
        , auditDetails = "Deposited via API"
        , auditSignature = Nothing
        }
      newReg = reg
        { regEntries = newEntries
        , regAudit = auditEntry : regAudit reg
        }
  saveRegistry newReg
  return newReg

-- | Withdraw a ruleset (get content and increment download count)
withdraw :: Text -> Maybe RuleVersion -> Registry -> IO (Maybe (Text, RegistryEntry))
withdraw name mVersion reg = case Map.lookup name (regEntries reg) of
  Nothing -> return Nothing
  Just entry -> do
    -- Check version if specified
    case mVersion of
      Just v | entryVersion entry /= v -> return Nothing
      _ -> do
        -- Increment download count
        now <- getCurrentTime
        let updatedEntry = entry { entryDownloads = entryDownloads entry + 1 }
            newEntries = Map.insert name updatedEntry (regEntries reg)
            auditEntry = AuditEntry
              { auditTimestamp = now
              , auditAction = AuditWithdraw
              , auditRuleset = name
              , auditVersion = Just (entryVersion entry)
              , auditActor = "system"
              , auditDetails = "Withdrawn via API"
              , auditSignature = Nothing
              }
            newReg = reg { regEntries = newEntries, regAudit = auditEntry : regAudit reg }
        saveRegistry newReg
        return $ Just (entryContent entry, updatedEntry)

-- | Withdraw latest version of a ruleset
withdrawLatest :: Text -> Registry -> IO (Maybe (Text, RegistryEntry))
withdrawLatest name = withdraw name Nothing

-- | Withdraw specific version of a ruleset
withdrawVersion :: Text -> RuleVersion -> Registry -> IO (Maybe (Text, RegistryEntry))
withdrawVersion name version = withdraw name (Just version)

-- ============================================================
-- Search Operations
-- ============================================================

-- | Search for rulesets matching query
search :: SearchQuery -> Registry -> SearchResult
search query@SearchQuery{..} reg =
  let allEntries = Map.elems (regEntries reg)
      -- Apply filters
      filtered = filter (matchesQuery query) allEntries
      -- Sort results
      sorted = sortResults querySortBy querySortDesc filtered
      -- Pagination
      total = length sorted
      paged = take queryLimit $ drop queryOffset sorted
      hasMore = queryOffset + queryLimit < total
  in SearchResult
       { resultEntries = paged
       , resultTotal = total
       , resultHasMore = hasMore
       , resultQuery = query
       }

-- | Check if entry matches query
matchesQuery :: SearchQuery -> RegistryEntry -> Bool
matchesQuery SearchQuery{..} entry =
  textMatch && effectMatch && categoryMatch && languageMatch &&
  tagsMatch && verifiedMatch && enabledMatch
  where
    textMatch = case queryText of
      Nothing -> True
      Just q -> T.isInfixOf (T.toLower q) (T.toLower $ entryName entry)
             || T.isInfixOf (T.toLower q) (T.toLower $ entryDescription entry)
    effectMatch = maybe True (== fromMaybe Preventive (entryEffect entry)) queryEffect
    categoryMatch = maybe True (\c -> entryCategory entry == Just c) queryCategory
    languageMatch = case queryLanguage of
      Nothing -> True
      Just l -> l `elem` entryLanguages entry
    tagsMatch = null queryTags || any (`elem` entryTags entry) queryTags
    verifiedMatch = maybe True (== entryVerified entry) queryVerified
    enabledMatch = maybe True (== entryEnabled entry) queryEnabled

-- | Sort search results
sortResults :: Text -> Bool -> [RegistryEntry] -> [RegistryEntry]
sortResults field desc entries = case field of
  "name" -> sorted (comparing entryName)
  "downloads" -> sorted (comparing entryDownloads)
  "updated" -> sorted (comparing entryUpdated)
  "created" -> sorted (comparing entryCreated)
  _ -> sorted (comparing entryName)
  where
    sorted cmp = if desc
      then sortBy (comparing Down . cmp) entries
      else sortBy cmp entries

-- | Search by effect type
searchByEffect :: Effect -> Registry -> [RegistryEntry]
searchByEffect effect reg =
  filter (\e -> entryEffect e == Just effect) (Map.elems $ regEntries reg)

-- | Search by language
searchByLanguage :: Language -> Registry -> [RegistryEntry]
searchByLanguage lang reg =
  filter (elem lang . entryLanguages) (Map.elems $ regEntries reg)

-- | Search by category
searchByCategory :: RulesetCategory -> Registry -> [RegistryEntry]
searchByCategory cat reg =
  filter (\e -> entryCategory e == Just cat) (Map.elems $ regEntries reg)

-- | Search by tags
searchByTags :: [Text] -> Registry -> [RegistryEntry]
searchByTags tags reg =
  filter (any (`elem` tags) . entryTags) (Map.elems $ regEntries reg)

-- ============================================================
-- Audit Operations
-- ============================================================

-- | Get full audit log for a ruleset
audit :: Text -> Registry -> AuditLog
audit name reg =
  let entries = filter ((== name) . auditRuleset) (regAudit reg)
      sorted = sortBy (comparing (Down . auditTimestamp)) entries
      created = if null sorted
                then error "No audit entries"
                else auditTimestamp (last sorted)
      updated = if null sorted
                then error "No audit entries"
                else auditTimestamp (head sorted)
  in AuditLog
       { logRuleset = name
       , logEntries = sorted
       , logCreated = created
       , logLastUpdated = updated
       }

-- | Get recent audit log entries
getAuditLog :: Int -> Registry -> [AuditEntry]
getAuditLog limit reg = take limit $ regAudit reg

-- | Record an audit entry
recordAudit :: AuditEntry -> Registry -> IO Registry
recordAudit entry reg = do
  let newReg = reg { regAudit = entry : regAudit reg }
  -- Persist audit entry
  let auditFile = regPath reg </> "audit" </> formatAuditFilename (auditTimestamp entry)
  TIO.writeFile auditFile (serializeAuditEntry entry)
  return newReg

-- ============================================================
-- Rule Management (legacy API)
-- ============================================================

-- | Register a new rule (legacy compatibility)
registerRule :: Text -> Text -> Text -> Registry -> IO Registry
registerRule name desc source reg =
  deposit name desc "" source Nothing reg

-- | Unregister a rule
unregisterRule :: Text -> Registry -> IO Registry
unregisterRule name reg = do
  now <- getCurrentTime
  let newEntries = Map.delete name (regEntries reg)
      auditEntry = AuditEntry
        { auditTimestamp = now
        , auditAction = AuditDelete
        , auditRuleset = name
        , auditVersion = Nothing
        , auditActor = "system"
        , auditDetails = "Unregistered"
        , auditSignature = Nothing
        }
      newReg = reg { regEntries = newEntries, regAudit = auditEntry : regAudit reg }
  saveRegistry newReg
  return newReg

-- | Look up a rule by name
lookupRule :: Text -> Registry -> Maybe RegistryEntry
lookupRule name reg = Map.lookup name (regEntries reg)

-- | List all rules
listRules :: Registry -> [RegistryEntry]
listRules reg = Map.elems (regEntries reg)

-- | Update a rule entry
updateRule :: Text -> (RegistryEntry -> RegistryEntry) -> Registry -> IO Registry
updateRule name f reg = case Map.lookup name (regEntries reg) of
  Nothing -> return reg
  Just entry -> do
    now <- getCurrentTime
    let updatedEntry = (f entry) { entryUpdated = now }
        newEntries = Map.insert name updatedEntry (regEntries reg)
        newReg = reg { regEntries = newEntries }
    saveRegistry newReg
    return newReg

-- ============================================================
-- Version Management
-- ============================================================

-- | Get current registry version
getVersion :: Registry -> RuleVersion
getVersion = regVersion

-- | Set registry version
setVersion :: RuleVersion -> Registry -> IO Registry
setVersion ver reg = do
  let newReg = reg { regVersion = ver }
  saveRegistry newReg
  return newReg

-- | Bump version (major, minor, or patch)
bumpVersion :: Text -> RuleVersion -> RuleVersion
bumpVersion component ver = case component of
  "major" -> ver { vMajor = vMajor ver + 1, vMinor = 0, vPatch = 0 }
  "minor" -> ver { vMinor = vMinor ver + 1, vPatch = 0 }
  "patch" -> bumpPatch ver
  _ -> bumpPatch ver

-- | Bump patch version
bumpPatch :: RuleVersion -> RuleVersion
bumpPatch ver = ver { vPatch = vPatch ver + 1 }

-- | Get rule change history
getRuleHistory :: Text -> Registry -> IO [Text]
getRuleHistory name reg = do
  let auditLog = audit name reg
  return $ map formatAuditEntry (logEntries auditLog)
  where
    formatAuditEntry e = T.pack $
      formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (auditTimestamp e) <>
      " - " <> show (auditAction e) <>
      maybe "" (\v -> " v" <> T.unpack (showVersion v)) (auditVersion e)

-- ============================================================
-- Version Helpers
-- ============================================================

-- | Show version as text
showVersion :: RuleVersion -> Text
showVersion RuleVersion{..} =
  T.pack $ show vMajor <> "." <> show vMinor <> "." <> show vPatch

-- | Parse version from text
parseVersion :: Text -> Maybe RuleVersion
parseVersion t = case map (reads . T.unpack) (T.splitOn "." t) of
  [[(maj, "")], [(min', "")], [(pat, "")]] ->
    Just $ RuleVersion maj min' pat
  _ -> Nothing

-- | Compare versions
compareVersions :: RuleVersion -> RuleVersion -> Ordering
compareVersions = compare

-- ============================================================
-- Serialization Helpers
-- ============================================================

serializeRegistry :: Map Text RegistryEntry -> RuleVersion -> Text
serializeRegistry entries ver = T.unlines $
  [ "# SPDX-License-Identifier: PLMP-1.0-or-later"
  , "# cicd-hyper-a Rule Registry"
  , "# Version: " <> showVersion ver
  , ""
  , "entries:"
  ] ++ map (\e -> "  - " <> entryName e) (Map.elems entries)

serializeEntry :: RegistryEntry -> Text
serializeEntry RegistryEntry{..} = T.unlines
  [ "# SPDX-License-Identifier: PLMP-1.0-or-later"
  , "name: " <> entryName
  , "version: " <> showVersion entryVersion
  , "description: " <> entryDescription
  , "source: " <> entrySource
  , "enabled: " <> if entryEnabled then "true" else "false"
  , "verified: " <> if entryVerified then "true" else "false"
  , "downloads: " <> T.pack (show entryDownloads)
  , "tags: " <> T.intercalate ", " entryTags
  , ""
  , "content: |"
  , T.unlines (map ("  " <>) (T.lines entryContent))
  ]

serializeAuditEntry :: AuditEntry -> Text
serializeAuditEntry AuditEntry{..} = T.unlines
  [ "timestamp: " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" auditTimestamp)
  , "action: " <> T.pack (show auditAction)
  , "ruleset: " <> auditRuleset
  , "version: " <> maybe "none" showVersion auditVersion
  , "actor: " <> auditActor
  , "details: " <> auditDetails
  ]

formatAuditFilename :: UTCTime -> FilePath
formatAuditFilename = formatTime defaultTimeLocale "%Y%m%d-%H%M%S.audit"

-- ============================================================
-- Loading Helpers
-- ============================================================

loadEntries :: FilePath -> IO (Map Text RegistryEntry)
loadEntries path = do
  exists <- doesFileExist path
  if exists
    then do
      files <- listDirectory path
      let rulesetFiles = filter ((== ".ruleset") . takeExtension) files
      entries <- forM rulesetFiles $ \file -> do
        content <- TIO.readFile (path </> file)
        return $ parseEntry content
      return $ Map.fromList [(entryName e, e) | Just e <- entries]
    else return Map.empty

loadAuditLog :: FilePath -> IO [AuditEntry]
loadAuditLog path = do
  exists <- doesFileExist path
  if exists
    then do
      files <- listDirectory path
      let auditFiles = filter ((== ".audit") . takeExtension) files
      entries <- forM auditFiles $ \file -> do
        content <- TIO.readFile (path </> file)
        return $ parseAuditEntry content
      return $ mapMaybe id entries
    else return []

-- Stub parsers (to be implemented with proper parsing)
parseEntry :: Text -> Maybe RegistryEntry
parseEntry _ = Nothing  -- TODO: Implement YAML parsing

parseAuditEntry :: Text -> Maybe AuditEntry
parseAuditEntry _ = Nothing  -- TODO: Implement YAML parsing
