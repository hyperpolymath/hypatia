// SPDX-License-Identifier: MPL-2.0
//! Timestamp wire shapes after the chrono -> jiff migration (RUSTSEC-2026-0543).
//!
//! The forge adapters deserialise API timestamps into `jiff::Timestamp` where
//! they used `chrono::DateTime<Utc>`. These are the shapes the forges send; each
//! must parse to the same instant chrono parsed it to, and what we serialise must
//! read back to the same instant.

use jiff::Timestamp;
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize, PartialEq)]
struct At {
    at: Timestamp,
}

fn parse(s: &str) -> Timestamp {
    serde_json::from_str::<At>(&format!(r#"{{"at":"{s}"}}"#))
        .unwrap_or_else(|e| panic!("{s} did not parse: {e}"))
        .at
}

#[test]
fn forge_shapes_parse_to_the_same_instant() {
    let utc = parse("2026-11-17T17:06:30Z").as_second();
    // GitHub, Codeberg/Gitea, SourceHut: second precision, `Z`.
    assert_eq!(utc, 1_794_935_190);
    // GitLab: milliseconds, `Z` or `+00:00`.
    assert_eq!(parse("2026-11-17T17:06:30.123Z").as_second(), utc);
    assert_eq!(parse("2026-11-17T17:06:30.123+00:00").as_second(), utc);
    // Bitbucket: microseconds, `+00:00`.
    assert_eq!(parse("2026-11-17T17:06:30.123456+00:00").as_second(), utc);
    // A non-UTC offset is the same instant, as chrono's DateTime<Utc> read it.
    assert_eq!(parse("2026-11-17T18:06:30+01:00").as_second(), utc);
    // Sub-second precision is kept, not truncated.
    assert_eq!(
        parse("2026-11-17T17:06:30.123456+00:00").subsec_microsecond(),
        123_456
    );
}

#[test]
fn a_naive_time_is_refused_as_chrono_refused_it() {
    // chrono's DateTime<Utc> deserialiser required an offset; so does jiff's.
    assert!(serde_json::from_str::<At>(r#"{"at":"2026-11-17T17:06:30"}"#).is_err());
}

#[test]
fn serialised_timestamps_round_trip_as_rfc3339() {
    let original = At {
        at: parse("2026-11-17T17:06:30.123456789Z"),
    };
    let json = serde_json::to_string(&original).unwrap();
    assert_eq!(json, r#"{"at":"2026-11-17T17:06:30.123456789Z"}"#);
    assert_eq!(serde_json::from_str::<At>(&json).unwrap(), original);
}

#[test]
fn epoch_seconds_convert_as_radicle_needs() {
    // radicle.rs: DateTime::from_timestamp(secs, 0).unwrap_or_default()
    // became Timestamp::from_second(secs).unwrap_or_default().
    assert_eq!(
        Timestamp::from_second(1_794_935_190).unwrap(),
        parse("2026-11-17T17:06:30Z")
    );
    assert_eq!(Timestamp::default(), Timestamp::UNIX_EPOCH);
}
