// SPDX-License-Identifier: AGPL-3.0-or-later
//! SHA pin reference for GitHub Actions
//! Generated from .audittraining/SHA-PINS.scm

use std::collections::HashMap;

/// SHA pins for commonly used GitHub Actions
pub struct ShaPins {
    pins: HashMap<String, (String, String)>, // action -> (sha, version)
}

impl ShaPins {
    pub fn new() -> Self {
        let mut pins = HashMap::new();

        // GitHub Official Actions
        pins.insert("actions/checkout".to_string(),
            ("8e8c483db84b4bee98b60c0593521ed34d9990e8".to_string(), "v6.0.1".to_string()));
        pins.insert("actions/upload-artifact".to_string(),
            ("b7c566a772e6b6bfb58ed0dc250532a479d7789f".to_string(), "v6.0.0".to_string()));
        pins.insert("actions/configure-pages".to_string(),
            ("983d7736d9b0ae728b81ab479565c72886d7745b".to_string(), "v5".to_string()));
        pins.insert("actions/upload-pages-artifact".to_string(),
            ("56afc609e74202658d3ffba0e8f6dda462b719fa".to_string(), "v3".to_string()));
        pins.insert("actions/deploy-pages".to_string(),
            ("d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e".to_string(), "v4".to_string()));
        pins.insert("actions/cache".to_string(),
            ("0c45773b623bea8c8e75f6c82b208c3cf94ea4f9".to_string(), "v4".to_string()));
        pins.insert("actions/jekyll-build-pages".to_string(),
            ("44a6e6beabd48582f863aeeb6cb2151cc1716697".to_string(), "v1".to_string()));

        // Security Actions
        pins.insert("github/codeql-action/init".to_string(),
            ("1b168cd39490f61582a9beae412bb7057a6b2c4e".to_string(), "v3.28.1".to_string()));
        pins.insert("github/codeql-action/analyze".to_string(),
            ("1b168cd39490f61582a9beae412bb7057a6b2c4e".to_string(), "v3.28.1".to_string()));
        pins.insert("github/codeql-action/upload-sarif".to_string(),
            ("1b168cd39490f61582a9beae412bb7057a6b2c4e".to_string(), "v3.28.1".to_string()));
        pins.insert("ossf/scorecard-action".to_string(),
            ("62b2cac7ed8198b15735ed49ab1e5cf35480ba46".to_string(), "v2.4.0".to_string()));
        pins.insert("trufflesecurity/trufflehog".to_string(),
            ("ef6e76c3c4023279497fab4721ffa071a722fd05".to_string(), "v3.92.4".to_string()));
        pins.insert("editorconfig-checker/action-editorconfig-checker".to_string(),
            ("9f8f6065f4db902c0c56cafa67cea18b3ebbb680".to_string(), "main".to_string()));

        // Rust Toolchain
        pins.insert("dtolnay/rust-toolchain".to_string(),
            ("6d9817901c499d6b02debbb57edb38d33daa680b".to_string(), "stable".to_string()));
        pins.insert("Swatinem/rust-cache".to_string(),
            ("779680da715d629ac1d338a641029a2f4372abb5".to_string(), "v2".to_string()));

        // Deno
        pins.insert("denoland/setup-deno".to_string(),
            ("11b63cf76cfcafb4e43f97b6cad24d8e8438f62d".to_string(), "v1".to_string()));

        // Erlang/Elixir
        pins.insert("erlef/setup-beam".to_string(),
            ("5304e04ea2b355f03681464e683d92e3b2f18451".to_string(), "v1.18.2".to_string()));

        // Other Tools
        pins.insert("extractions/setup-just".to_string(),
            ("e33e0265a09d6d736e2ee1e0eb685ef1de4669ff".to_string(), "v3.0.0".to_string()));
        pins.insert("extractions/setup-crate".to_string(),
            ("4993624604c307fbca528d28a3c8b60fa5ecc859".to_string(), "v1".to_string()));
        pins.insert("ruby/setup-ruby".to_string(),
            ("4a9ddd6f338a97768b8006bf671dfbad383215f4".to_string(), "v1.207.0".to_string()));
        pins.insert("ocaml/setup-ocaml".to_string(),
            ("4c1df9105efb7a8b996c21e052e4fb8b64a8f2fc".to_string(), "v3".to_string()));
        pins.insert("webfactory/ssh-agent".to_string(),
            ("dc588b651fe13675774614f8e6a936a468676387".to_string(), "v0.9.0".to_string()));
        pins.insert("softprops/action-gh-release".to_string(),
            ("da05d552573ad5aba039eaac05058a918a7bf631".to_string(), "v2".to_string()));

        // ClusterFuzzLite
        pins.insert("google/clusterfuzzlite/actions/build_fuzzers".to_string(),
            ("884713a6c30a92e5e8544c39945cd7cb630abcd1".to_string(), "v1".to_string()));
        pins.insert("google/clusterfuzzlite/actions/run_fuzzers".to_string(),
            ("884713a6c30a92e5e8544c39945cd7cb630abcd1".to_string(), "v1".to_string()));

        Self { pins }
    }

    /// Get SHA pin for an action
    pub fn get_pin(&self, action: &str) -> Option<(String, String)> {
        self.pins.get(action).cloned()
    }

    /// Get the replacement string for a workflow file
    pub fn get_replacement(&self, action: &str) -> Option<String> {
        self.pins.get(action).map(|(sha, ver)| format!("{}@{} # {}", action, sha, ver))
    }

    /// Check if an action has a known SHA pin
    pub fn has_pin(&self, action: &str) -> bool {
        self.pins.contains_key(action)
    }

    /// Get all known actions
    pub fn all_actions(&self) -> Vec<&String> {
        self.pins.keys().collect()
    }
}

impl Default for ShaPins {
    fn default() -> Self {
        Self::new()
    }
}
