// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Hypatia V-lang API — Neurosymbolic CI/CD scanner client.
module hypatia

pub enum RuleSeverity {
	info
	warning
	@error
	critical
}

pub enum ScanResult {
	pass
	fail
	skip
	@error
}

pub struct Finding {
pub:
	rule_id  string
	severity RuleSeverity
	file     string
	line     int
	message  string
}

pub struct ScanRequest {
pub:
	repo_path string
	rules     []string // empty = all rules
}

pub struct ScanResponse {
pub:
	result   ScanResult
	findings []Finding
	score    int // 0-100 (bounded by ABI)
}

fn C.hypatia_scan(path_ptr &u8, rule_count int) int
fn C.hypatia_clamp_score(score int) int
fn C.hypatia_rule_count() int

// scan runs Hypatia analysis on a repository path.
pub fn scan(req ScanRequest) ScanResponse {
	result := C.hypatia_scan(req.repo_path.str, req.rules.len)
	return ScanResponse{
		result: unsafe { ScanResult(result) }
		findings: []
		score: 0
	}
}

// clamp_score ensures a score is within [0, 100].
pub fn clamp_score(score int) int {
	return C.hypatia_clamp_score(score)
}

// rule_count returns the number of registered scan rules.
pub fn rule_count() int {
	return C.hypatia_rule_count()
}
