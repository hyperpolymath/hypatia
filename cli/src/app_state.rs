// SPDX-License-Identifier: PMPL-1.0-or-later
//! Shared application state for the cicd-hyper-a CLI.
//!
//! `AppState` is the single owner of runtime-mutable configuration that must
//! be shared across command handlers without cloning the entire `Config`.
//!
//! The `mode_selector` field drives how Hypatia interacts with pull requests:
//!
//! | Mode        | Behaviour                                                   |
//! |-------------|-------------------------------------------------------------|
//! | `Verifier`  | Runs checks, no GitHub interaction                         |
//! | `Advisor`   | Posts a PR comment with proof suggestions                   |
//! | `Consultant`| Posts a PR *review* comment on the offending lines          |
//! | `Regulator` | Sets the required check-run to failure, blocking the merge  |

#![allow(dead_code)]

use crate::config::{BotMode, Config};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

/// Per-slot rate-limiter state (requests remaining + window reset time).
struct RateLimitSlot {
    remaining: AtomicU32,
    window_start: std::sync::Mutex<Instant>,
    window_size: Duration,
    limit: u32,
}

impl RateLimitSlot {
    fn new(limit: u32, window: Duration) -> Self {
        Self {
            remaining: AtomicU32::new(limit),
            window_start: std::sync::Mutex::new(Instant::now()),
            window_size: window,
            limit,
        }
    }

    /// Returns `true` if the request is allowed (consumes one token).
    fn try_acquire(&self) -> bool {
        let now = Instant::now();
        let mut start = self.window_start.lock().unwrap();
        if now.duration_since(*start) >= self.window_size {
            self.remaining.store(self.limit, Ordering::Relaxed);
            *start = now;
        }
        let prev = self
            .remaining
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |r| {
                if r > 0 {
                    Some(r - 1)
                } else {
                    None
                }
            });
        prev.is_ok()
    }
}

/// Token-bucket rate limiter used by the dispatcher.
///
/// Three independent windows mirror the Elixir `RateLimiter` GenServer:
/// - per-bot: 50 requests / 60 s
/// - global:  200 requests / 60 s
/// - burst:   10 requests /  5 s
pub struct RateLimiter {
    per_bot: RateLimitSlot,
    global: RateLimitSlot,
    burst: RateLimitSlot,
}

impl RateLimiter {
    /// Create a rate limiter with production defaults.
    pub fn new() -> Self {
        Self {
            per_bot: RateLimitSlot::new(50, Duration::from_secs(60)),
            global: RateLimitSlot::new(200, Duration::from_secs(60)),
            burst: RateLimitSlot::new(10, Duration::from_secs(5)),
        }
    }

    /// Returns `true` if all three windows allow the dispatch.
    pub fn allow(&self) -> bool {
        self.burst.try_acquire() && self.per_bot.try_acquire() && self.global.try_acquire()
    }
}

impl Default for RateLimiter {
    fn default() -> Self {
        Self::new()
    }
}

/// Shared runtime state for the CLI and command handlers.
///
/// Cheap to clone — inner data is reference-counted.
#[derive(Clone)]
pub struct AppState {
    /// Resolved configuration (merged from files + env vars).
    pub config: Arc<Config>,

    /// Dispatch rate limiter (per-bot / global / burst windows).
    pub rate_limiter: Arc<RateLimiter>,

    /// Active operating mode, read from `config.bot.mode` at construction.
    ///
    /// Drives GitHub interaction pattern: verifier → advisor → consultant → regulator.
    pub mode_selector: BotMode,
}

impl AppState {
    /// Construct from a loaded `Config`. The `mode_selector` is extracted once
    /// at construction so command handlers can read it without locking.
    pub fn new(config: Config) -> Self {
        let mode_selector = config.bot.mode;
        Self {
            config: Arc::new(config),
            rate_limiter: Arc::new(RateLimiter::new()),
            mode_selector,
        }
    }

    /// Whether the bot should post any GitHub comments.
    pub fn posts_comments(&self) -> bool {
        matches!(
            self.mode_selector,
            BotMode::Advisor | BotMode::Consultant | BotMode::Regulator
        )
    }

    /// Whether the bot should block merges by failing a required check-run.
    pub fn blocks_merge(&self) -> bool {
        matches!(self.mode_selector, BotMode::Regulator)
    }

    /// Whether the bot posts inline PR review comments (line-level).
    pub fn posts_review_comments(&self) -> bool {
        matches!(self.mode_selector, BotMode::Consultant | BotMode::Regulator)
    }
}
