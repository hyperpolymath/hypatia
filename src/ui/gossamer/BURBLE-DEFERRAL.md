# ADR: Burble session integration deferred

**Date:** 2026-04-22
**Status:** Accepted
**Issue:** #177 follow-up

## Decision

Hypatia's Gossamer GUI ships without Burble session integration.
All UI↔backend traffic goes through Gossamer's direct IPC channel
(`ipc_open("http://localhost:9090/ws")` in `bridge.eph:132`), not
through a Burble P2P session.

## Rationale

- **Burble's Ephapax binding does not exist yet.** Burble is
  implemented in Elixir (signalling) + JavaScript (client) + Zig
  (audio coprocessor NIFs). An Ephapax FFI layer has not been
  published upstream.
- **Single-operator use is the current shape.** Hypatia is run by one
  operator against one Elixir backend. The P2P multi-operator
  scenarios Burble enables (voice control plane, cross-session
  coordination) are not on the current roadmap.
- **Direct IPC is sufficient.** The WebSocket channel to Bandit on
  :9090 already covers every UI message path. Adding Burble now would
  add a transport layer with no user-visible benefit.

## Deferral triggers

Promote Burble to a required integration when any of the following
lands:

1. **PanLL publishes Hypatia state as a Burble topic.** The moment
   PanLL's dashboard expects Hypatia to emit findings/dispatches via
   a Burble session rather than via a direct HTTP read of the harness
   endpoints.
2. **Multi-operator session.** Two or more operators need to share
   Hypatia state (e.g. a review seat watching the safety triangle
   live while another operator drives dispatches).
3. **Voice control reaches the GUI.** Burble's voice-control plane
   wants to fire `Msg.Navigate(Department.Verification)` or similar
   from outside the browser.

## Minimal shim (when promoted)

Uncomment the `extern "burble"` block sketched in `bridge.eph:76-89`
and replace the `Session` type / three-function signature with the
real Burble Ephapax module when published. The TEA loop in `run/3`
already uses linear handles (`let!`) so a `Session` handle drops in
without structural change.

## What this ADR does NOT say

- The Burble project is wrong or unwanted. It is load-bearing for the
  broader PanLL ambient-computing vision.
- Hypatia will never use Burble. The triggers above describe exactly
  the conditions under which this decision is revisited.
- The `[burble]` section in the earlier `panel.manifest.toml` was
  binding. That file has been replaced by `panll.harness.toml`
  (schema-validated); the burble section went with it.
