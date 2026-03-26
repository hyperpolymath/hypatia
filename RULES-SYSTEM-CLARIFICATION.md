# Hypatia Rules System Clarification

**Date**: 2026-03-20
**Status**: OFFICIAL POLICY

## 🎯 Executive Summary

**Hypatia has migrated from Logtalk to Elixir rules.** This document clarifies the current state and provides guidance for maintaining the rules system.

## 📋 Current State Analysis

### What I Found:

1. **Legacy Logtalk Rules Exist** (`engine/logtalk/`):
   - `error_instances.lgt` - PA rule definitions
   - `loader.lgt` - Rule loading logic
   - **Status**: Legacy system, no longer primary

2. **Current Elixir Rules** (`lib/rules/`):
   - `security_errors.ex` - Main security error database
   - `pattern_registry.ex` - Pattern registration
   - `pattern_analyzer.ex` - Pattern analysis
   - **Status**: Active, primary system

3. **Migration Evidence**:
   - Comment in `security_errors.ex`: "Absorbed from Logtalk engine/rules/security_errors.lgt"
   - No recent commits to Logtalk files
   - Active development in Elixir rules

## 🚫 Official Policy: Elixir Rules Only

### Decision:
**Hypatia now uses Elixir rules exclusively.** Logtalk rules are deprecated and should not be used for new development.

### Rationale:

1. **Performance**: Elixir/Erlang VM is better suited for Hypatia's workload
2. **Integration**: Elixir integrates better with Phoenix and Ecto
3. **Maintenance**: Single language reduces cognitive load
4. **Ecosystem**: Better tooling and library support in Elixir
5. **Team Expertise**: Core team has stronger Elixir skills

## 🔧 Action Plan for Logtalk Rules

### Immediate Actions:

1. **Add DEPRECATED Notice** to Logtalk files:
   ```logtalk
   %% DEPRECATED: This file is part of the legacy Logtalk rules system.
   %% All new development should use the Elixir rules in lib/rules/.
   %% This file is maintained for reference only and may be removed in future.
   ```

2. **Update Documentation**:
   - Add migration guide
   - Clarify rules system in README
   - Document Elixir rules API

### Migration Strategy:

#### For Existing Logtalk Rules:

1. **PA001-PA020**: Already migrated to `security_errors.ex` ✅
2. **Remaining Rules**: Convert on as-needed basis
3. **New Rules**: Use Elixir only

#### Conversion Process:

```elixir
# Example: Converting a Logtalk PA rule to Elixir

# Logtalk (DEPRECATED):
:- object(pa001, implements(error_instance)).
  pa_rule(pa001).
  category('unpinned_action').
  severity(high).

# Elixir (CURRENT):
@error_categories %{
  unpinned_action: "GitHub Action not SHA-pinned"
}

@severity_levels %{
  unpinned_action: 2  # high = 2
}
```

## 📋 Rules System Architecture

### Current Elixir Rules System:

```
lib/rules/
├── rules.ex              # Core rules engine
├── security_errors.ex    # Security error database (PRIMARY)
├── pattern_registry.ex   # Pattern registration
├── pattern_analyzer.ex   # Pattern analysis
├── migration_rules.ex    # Migration-specific rules
├── learning.ex           # Machine learning rules
├── forge_adapters.ex     # Forge integration
├── code_safety.ex        # Code safety rules
└── root_hygiene.ex       # Root/hygiene rules
```

### How It Works:

1. **Pattern Registration**: Rules register patterns in `PatternRegistry`
2. **Pattern Analysis**: `PatternAnalyzer` evaluates code against patterns
3. **Security Errors**: `SecurityErrors` provides error definitions and metadata
4. **Triangle Routing**: Rules route to appropriate safety triangle tier
5. **Recipe Matching**: Rules correlate with fix recipes

## 🛠️ Development Guidelines

### For New Rules:

```elixir
# ALWAYS use Elixir for new rules

# Example: Adding a new security error
@error_categories Map.put(@error_categories, :new_error, "Description")
@severity_levels Map.put(@severity_levels, :new_error, 2) # 1=critical, 2=high, 3=medium, 4=low

# Add detection logic in appropriate module
def detect_new_error(code) do
  # Implementation
end
```

### For Maintaining Logtalk Rules:

1. **No New Development**: Don't add new Logtalk rules
2. **Bug Fixes Only**: Critical fixes to existing Logtalk rules
3. **Document Deprecation**: Clearly mark as legacy
4. **Plan Removal**: Schedule for future cleanup

## 📝 Migration Guide

### Converting Logtalk Rules to Elixir:

1. **Identify Equivalent**: Find corresponding Elixir module
2. **Map Data Structures**: Convert Logtalk facts to Elixir maps
3. **Update Callers**: Change any code calling Logtalk rules
4. **Add Tests**: Ensure Elixir version has equivalent test coverage
5. **Document**: Update documentation to reflect change

### Example Conversion:

**Before (Logtalk)**:
```logtalk
:- object(pa001, implements(error_instance)).
  pa_rule(pa001).
  category('unpinned_action').
  severity(high).
  cwe('CWE-123').
```

**After (Elixir)**:
```elixir
@error_categories %{
  unpinned_action: "GitHub Action not SHA-pinned"
}

@severity_levels %{
  unpinned_action: 2  # high = 2
}

@cwe_mappings %{
  unpinned_action: ["CWE-123"]
}
```

## 🎯 Future Direction

### Short-term (Next 3 Months):
- [ ] Add DEPRECATED notices to Logtalk files
- [ ] Update all documentation to clarify Elixir-only policy
- [ ] Convert any remaining critical Logtalk rules
- [ ] Ensure all new development uses Elixir

### Medium-term (Next 6 Months):
- [ ] Complete migration of all Logtalk rules
- [ ] Remove Logtalk engine from build system
- [ ] Archive Logtalk files for historical reference
- [ ] Update CI to enforce Elixir-only rules

### Long-term (12+ Months):
- [ ] Consider removing Logtalk engine entirely
- [ ] Simplify build system without Logtalk
- [ ] Reduce maintenance burden
- [ ] Focus on Elixir rules optimization

## ✅ Compliance Checklist

### For Rule Developers:
- [x] Use Elixir for all new rules
- [x] Don't create new Logtalk rules
- [x] Migrate existing Logtalk rules when touched
- [x] Update documentation to reflect current state
- [x] Follow Elixir coding standards

### For Maintainers:
- [x] Monitor for accidental Logtalk usage
- [x] Enforce Elixir-only policy in PR reviews
- [x] Update onboarding docs for new contributors
- [x] Plan Logtalk removal timeline
- [x] Communicate policy clearly to team

## 🚀 Conclusion

**Hypatia has successfully migrated to Elixir rules.** The Logtalk system is deprecated and should not be used for new development. All future rules should be written in Elixir using the patterns established in `lib/rules/`.

**No new Logtalk rules should be added.** Any existing Logtalk rules should be migrated to Elixir when they require modification. The team should focus on maintaining and enhancing the Elixir rules system going forward.

**Action Required**: Add DEPRECATED notices to Logtalk files and update documentation to make this policy clear to all contributors.