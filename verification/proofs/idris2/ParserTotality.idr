-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- ParserTotality.idr — totality and round trips of the VCL FROM-clause parser.
--
-- Re-homed from verification/proofs/lean4/ParserTotality.lean (PROOF-POLICY
-- section 3). The statements are the Lean file's; the Lean proofs rested on
-- the kernel axiom propext, which Idris2 does not have.
--
-- Models a representative subset of `parse_from/1` in lib/vcl/: FROM STORE,
-- FROM FEDERATION and FROM HEXAD, each followed by a bare identifier or (for
-- STORE and FEDERATION) a quoted literal; anything else is an error. The
-- model omits unquoted URLs, WITH DRIFT and multi-URL sources. It does not
-- show that the Elixir parser follows it.

module ParserTotality

%default total

------------------------------------------------------------------------
-- Section 1: tokens, sources, errors, results
------------------------------------------------------------------------

||| Tokens from the VCL tokenizer; quoted literals are kept apart from bare
||| identifiers so the parser's two paths can be modelled separately.
public export
data Token
  = KwFrom
  | KwStore
  | KwFederation
  | KwHexad
  | Ident String
  | Quoted String
  | Other String

||| The parsed source: {:store, id}, {:federation, pattern}, {:hexad, uuid}.
public export
data Source = Store String | Federation String | Hexad String

||| Parser errors, structured for the proofs.
public export
data ParseError = ExpectedFromClause | EmptyInput | Malformed

||| A result: a value and the tokens left over, or an error.
public export
data ParseResult a = Ok a (List Token) | Err ParseError

------------------------------------------------------------------------
-- Section 2: the parser
------------------------------------------------------------------------

||| Parse FROM STORE or FROM FEDERATION with a bare or quoted value, or
||| FROM HEXAD with a bare value. Return the source and unconsumed tokens.
||| Empty input returns Err EmptyInput; any other unmatched input returns
||| Err ExpectedFromClause. `%default total` makes the checker refuse this
||| definition unless it covers every input and terminates, so checking the
||| definition is the totality proof.
public export
parseFrom : List Token -> ParseResult Source
parseFrom (KwFrom :: KwStore :: Ident id :: rest)        = Ok (Store id) rest
parseFrom (KwFrom :: KwStore :: Quoted id :: rest)       = Ok (Store id) rest
parseFrom (KwFrom :: KwFederation :: Ident pat :: rest)  = Ok (Federation pat) rest
parseFrom (KwFrom :: KwFederation :: Quoted pat :: rest) = Ok (Federation pat) rest
parseFrom (KwFrom :: KwHexad :: Ident u :: rest)         = Ok (Hexad u) rest
parseFrom []                                             = Err EmptyInput
parseFrom _                                              = Err ExpectedFromClause

------------------------------------------------------------------------
-- Section 3: totality as a structural property
------------------------------------------------------------------------

||| Property 1: parseFrom is a function: equal inputs give equal results.
export
parseFromDeterministic : (ts : List Token) -> parseFrom ts = parseFrom ts
parseFromDeterministic _ = Refl

||| Property 2: empty input gives the empty-input error.
export
parseFromEmpty : parseFrom [] = Err EmptyInput
parseFromEmpty = Refl

------------------------------------------------------------------------
-- Section 4: round trips on well-formed input
------------------------------------------------------------------------

||| Property 4: FROM STORE <ident> gives a store.
export
parseFromStoreBare : parseFrom [KwFrom, KwStore, Ident "scans"] = Ok (Store "scans") []
parseFromStoreBare = Refl

||| Property 5: FROM STORE "quoted" gives a store.
export
parseFromStoreQuoted : parseFrom [KwFrom, KwStore, Quoted "scans"] = Ok (Store "scans") []
parseFromStoreQuoted = Refl

||| Property 6: FROM FEDERATION <ident> gives a federation, pattern kept.
export
parseFromFederation : parseFrom [KwFrom, KwFederation, Ident "/scans/*"]
                      = Ok (Federation "/scans/*") []
parseFromFederation = Refl

||| Property 7: FROM HEXAD <uuid> gives a hexad.
export
parseFromHexad : parseFrom [KwFrom, KwHexad, Ident "abc-123"] = Ok (Hexad "abc-123") []
parseFromHexad = Refl

||| Property 8: tokens after a valid FROM clause are passed on, so the
||| WHERE / LIMIT / OFFSET parsers still see them.
export
parseFromPassesRemainder :
  parseFrom [KwFrom, KwStore, Ident "scans", Other "WHERE", Other "FIELD"]
  = Ok (Store "scans") [Other "WHERE", Other "FIELD"]
parseFromPassesRemainder = Refl

------------------------------------------------------------------------
-- Section 5: malformed input is rejected
------------------------------------------------------------------------

||| Property 9: FROM with no STORE / FEDERATION / HEXAD is rejected.
export
parseFromOrphanFrom : parseFrom [KwFrom] = Err ExpectedFromClause
parseFromOrphanFrom = Refl

||| Property 10: a clause without FROM is rejected.
export
parseFromNoFromKeyword : parseFrom [KwStore, Ident "scans"] = Err ExpectedFromClause
parseFromNoFromKeyword = Refl

------------------------------------------------------------------------
-- Section 6: a Bool view of success
------------------------------------------------------------------------

||| Did the parse succeed?
public export
parseFromSucceeded : List Token -> Bool
parseFromSucceeded ts = case parseFrom ts of
  Ok _ _ => True
  Err _  => False

||| Property 11: the predicate agrees with the parser on FROM STORE.
export
parseFromSucceededStore : parseFromSucceeded [KwFrom, KwStore, Ident "scans"] = True
parseFromSucceededStore = Refl

||| Property 12: the predicate is false on empty input.
export
parseFromSucceededEmpty : parseFromSucceeded [] = False
parseFromSucceededEmpty = Refl

------------------------------------------------------------------------
-- Section 7: the remainder and rejection properties for every input
------------------------------------------------------------------------
-- Sections 4 and 5 check single inputs. The properties below quantify over
-- every value and every remainder, so the ledger's obligation rests on them.

||| Property 13: every well-formed clause passes its remainder on unchanged,
||| for every value and every remainder.
export
parseFromStoreAny : (v : String) -> (rest : List Token) ->
  parseFrom (KwFrom :: KwStore :: Ident v :: rest) = Ok (Store v) rest
parseFromStoreAny _ _ = Refl

export
parseFromStoreQuotedAny : (v : String) -> (rest : List Token) ->
  parseFrom (KwFrom :: KwStore :: Quoted v :: rest) = Ok (Store v) rest
parseFromStoreQuotedAny _ _ = Refl

export
parseFromFederationAny : (v : String) -> (rest : List Token) ->
  parseFrom (KwFrom :: KwFederation :: Ident v :: rest) = Ok (Federation v) rest
parseFromFederationAny _ _ = Refl

export
parseFromFederationQuotedAny : (v : String) -> (rest : List Token) ->
  parseFrom (KwFrom :: KwFederation :: Quoted v :: rest) = Ok (Federation v) rest
parseFromFederationQuotedAny _ _ = Refl

export
parseFromHexadAny : (v : String) -> (rest : List Token) ->
  parseFrom (KwFrom :: KwHexad :: Ident v :: rest) = Ok (Hexad v) rest
parseFromHexadAny _ _ = Refl

||| A token that is not FROM.
public export
data NotFrom : Token -> Type where
  NFStore      : NotFrom KwStore
  NFFederation : NotFrom KwFederation
  NFHexad      : NotFrom KwHexad
  NFIdent      : NotFrom (Ident s)
  NFQuoted     : NotFrom (Quoted s)
  NFOther      : NotFrom (Other s)

||| Property 14: input that does not start with FROM is rejected, whatever
||| follows.
export
parseFromRejectsNoFrom : (t : Token) -> (ts : List Token) -> NotFrom t ->
  parseFrom (t :: ts) = Err ExpectedFromClause
parseFromRejectsNoFrom KwStore _ NFStore = Refl
parseFromRejectsNoFrom KwFederation _ NFFederation = Refl
parseFromRejectsNoFrom KwHexad _ NFHexad = Refl
parseFromRejectsNoFrom (Ident _) _ NFIdent = Refl
parseFromRejectsNoFrom (Quoted _) _ NFQuoted = Refl
parseFromRejectsNoFrom (Other _) _ NFOther = Refl

||| A token that cannot follow FROM: not STORE, FEDERATION or HEXAD.
public export
data NotSourceKw : Token -> Type where
  NSFrom   : NotSourceKw KwFrom
  NSIdent  : NotSourceKw (Ident s)
  NSQuoted : NotSourceKw (Quoted s)
  NSOther  : NotSourceKw (Other s)

||| Property 15: FROM followed by anything but a source keyword is rejected,
||| whatever follows.
export
parseFromRejectsBadSource : (t : Token) -> (ts : List Token) -> NotSourceKw t ->
  parseFrom (KwFrom :: t :: ts) = Err ExpectedFromClause
parseFromRejectsBadSource KwFrom _ NSFrom = Refl
parseFromRejectsBadSource (Ident _) _ NSIdent = Refl
parseFromRejectsBadSource (Quoted _) _ NSQuoted = Refl
parseFromRejectsBadSource (Other _) _ NSOther = Refl

||| Property 16: a source keyword with nothing after it is rejected.
export
parseFromRejectsTruncated :
  ( parseFrom [KwFrom, KwStore] = Err ExpectedFromClause
  , parseFrom [KwFrom, KwFederation] = Err ExpectedFromClause
  , parseFrom [KwFrom, KwHexad] = Err ExpectedFromClause )
parseFromRejectsTruncated = (Refl, Refl, Refl)
