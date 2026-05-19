// SPDX-License-Identifier: PMPL-1.0-or-later
//
// validate-panll-harness — load a panll harness manifest (TOML), validate it
// against the panll-harness/v2 JSON-Schema (Draft 2020-12), fail CI if it
// does not conform. A faithful Rust port of the inline `python3` +
// `jsonschema` step formerly embedded in build-gossamer-gui.yml (org policy
// bans Python outside SaltStack; no exceptions).
//
// Usage:  validate-panll-harness <harness.toml> <schema.json>
// Exit:   0 = valid, 1 = invalid / schema error, 2 = usage / IO error.

use std::process::exit;

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    if argv.len() != 3 {
        eprintln!("usage: validate-panll-harness <harness.toml> <schema.json>");
        exit(2);
    }
    let toml_path = &argv[1];
    let schema_path = &argv[2];

    let toml_text = match std::fs::read_to_string(toml_path) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("error: cannot read {toml_path}: {e}");
            exit(2);
        }
    };
    let schema_text = match std::fs::read_to_string(schema_path) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("error: cannot read {schema_path}: {e}");
            exit(2);
        }
    };

    // TOML -> serde_json::Value (the JSON data model the validator expects),
    // mirroring Python's `tomllib.load` feeding `jsonschema.validate`.
    let toml_value: toml::Value = match toml::from_str(&toml_text) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("error: {toml_path} is not valid TOML: {e}");
            exit(2);
        }
    };
    let instance: serde_json::Value = match serde_json::to_value(&toml_value) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("error: cannot convert {toml_path} to JSON: {e}");
            exit(2);
        }
    };
    let schema: serde_json::Value = match serde_json::from_str(&schema_text) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("error: {schema_path} is not valid JSON: {e}");
            exit(2);
        }
    };

    // Building with the 2020-12 draft also meta-validates the schema itself,
    // covering Python's explicit `Draft202012Validator.check_schema(schema)`.
    let validator = match jsonschema::options()
        .with_draft(jsonschema::Draft::Draft202012)
        .build(&schema)
    {
        Ok(v) => v,
        Err(e) => {
            println!(
                "::error::panll.harness.toml fails panll-harness/v2 \
                 validation: invalid schema: {e}"
            );
            exit(1);
        }
    };

    let errors: Vec<String> = validator
        .iter_errors(&instance)
        .map(|e| format!("{e} (at {})", e.instance_path()))
        .collect();

    if !errors.is_empty() {
        println!(
            "::error::panll.harness.toml fails panll-harness/v2 \
             validation: {}",
            errors.join("; ")
        );
        exit(1);
    }

    println!("OK  panll.harness.toml validates against panll-harness/v2");
}
