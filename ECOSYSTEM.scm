;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Ecosystem relationships for hypatia
;; Media type: application/vnd.ecosystem+scm

(ecosystem
  (metadata
    ((version . "1.0.0")
     (name . "hypatia")
     (type . "validation-platform")
     (purpose . "Part of hyperpolymath tool ecosystem")))
  
  (position-in-ecosystem
    "Provides validation-platform functionality within the hyperpolymath suite")
  
  (related-projects
    ((echidnabot . "automation-provider")
     (scaffoldia . "template-consumer")))
  
  (what-this-is
    "hypatia is a specialized tool in the hyperpolymath ecosystem")
  
  (what-this-is-not
    "Not a general-purpose framework"
    "Not intended as standalone product"))
