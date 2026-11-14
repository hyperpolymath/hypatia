---
# SPDX-License-Identifier: CC-BY-SA-4.0
name: claim-receipt
description: State a result with its evidence block (commit, commands run with copied output, CI link, what did not run). Use before saying that something is verified, fixed, passing, green or done, and when writing the Evidence part of a pull request.
license: CC-BY-SA-4.0
---

# State a result with its receipt

A claim with no receipt is an assertion. Report it as "not verified".

## The block

```text
Claim:      <one sentence>
Head:       <full commit sha that the checks ran on>
Ran:        <command>            -> <result line, copied>
CI:         <run link>           -> <conclusion>, or "not run"
Read only:  <what you concluded from the code without a run>
Not run:    <each check that the claim needs and that did not run, and why>
Status:     verified | partly verified | not verified
```

The template with its rules is
`docs/operations/e0-closeout/templates/result-receipt.adoc`.

## Rules

- `verified` needs each check under "Ran" to pass on the commit under "Head",
  and nothing under "Not run". A claim with a check under "Not run" is
  `partly verified` at most.
- Copy the result line from the tool output. Do not write it from memory.
- List a check that did not run under "Not run". Do not leave it out.
- A skipped gate is not a pass. A zero exit code alone is not a pass. A test
  count of zero is not a pass.
- When the head moves, the receipt is stale. Run the checks again.
- Keep "read" and "ran" apart. "I read the function and it returns X" is a
  weaker statement than "I ran the test and it returned X".
- Give a count with its denominator: "22 of 42", not a percentage alone.

## Words

Use `verified` only with a receipt. For the rest, say what you did: "read",
"ran", "measured", "not checked". Do not use "should work" or "looks correct"
as a result.
