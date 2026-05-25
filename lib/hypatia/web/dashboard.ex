# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Web.Dashboard do
  @moduledoc """
  Single-page operational dashboard for Hypatia.

  Served at `GET /` by `Hypatia.Web.Router`. Plain HTML + vanilla JS
  + CSS — no build step, no framework, no node_modules. The page
  polls `/api/status` every 2s for the counters / queue depths, and
  opens an `EventSource` connection to `/api/events` for live
  telemetry as it fires.

  Loopback-only by design — uses the same gate as the rest of /api/*
  (the gate plug runs in `ApiRouter`; the dashboard makes XHR/SSE
  calls to those endpoints, so a non-local browser would be rejected
  by the API endpoints even if it reached the dashboard).

  The HTML is generated at compile-time (module attribute) so there's
  no template-rendering cost on each request and no on-disk asset
  file to misplace.
  """

  import Plug.Conn

  @html ~S"""
  <!DOCTYPE html>
  <html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Hypatia — Live Watcher</title>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <style>
      :root {
        color-scheme: light dark;
        --bg: #0f1419;
        --fg: #e6e1cf;
        --dim: #8b949e;
        --accent: #6cb6ff;
        --warn: #e3b341;
        --bad: #f47067;
        --good: #56d364;
        --grid: #1e242a;
        --card: #161b22;
      }
      * { box-sizing: border-box; }
      body {
        margin: 0;
        padding: 1.5rem;
        font-family: ui-monospace, "SF Mono", Consolas, "Liberation Mono", monospace;
        background: var(--bg);
        color: var(--fg);
        font-size: 13px;
        line-height: 1.4;
      }
      h1, h2 { font-weight: 600; margin: 0; }
      h1 { font-size: 1.4rem; margin-bottom: 0.25rem; }
      h2 { font-size: 0.95rem; color: var(--accent); margin: 1rem 0 0.5rem; text-transform: uppercase; letter-spacing: 0.05em; }
      .header { display: flex; justify-content: space-between; align-items: baseline; border-bottom: 1px solid var(--grid); padding-bottom: 0.75rem; margin-bottom: 1rem; }
      .meta { color: var(--dim); font-size: 0.85rem; }
      .grid { display: grid; grid-template-columns: 1fr 1fr; gap: 1rem; }
      .card { background: var(--card); border: 1px solid var(--grid); border-radius: 6px; padding: 0.75rem 1rem; }
      table { width: 100%; border-collapse: collapse; }
      th, td { text-align: left; padding: 0.25rem 0.5rem; border-bottom: 1px solid var(--grid); }
      th { color: var(--dim); font-weight: normal; }
      td.num { text-align: right; font-variant-numeric: tabular-nums; }
      td.warn { color: var(--warn); }
      td.bad  { color: var(--bad); }
      td.good { color: var(--good); }
      td.dim  { color: var(--dim); }
      .pill {
        display: inline-block;
        padding: 0 0.4em;
        border-radius: 3px;
        background: var(--grid);
        color: var(--dim);
        font-size: 0.8rem;
      }
      .pill.good { background: rgba(86,211,100,0.15); color: var(--good); }
      .pill.warn { background: rgba(227,179,65,0.15); color: var(--warn); }
      .pill.bad  { background: rgba(244,112,103,0.15); color: var(--bad); }
      .stream {
        font-size: 0.85rem;
        max-height: 320px;
        overflow-y: auto;
      }
      .stream .ev { padding: 0.2rem 0; border-bottom: 1px solid var(--grid); }
      .stream .ev:last-child { border: none; }
      .stream .ts { color: var(--dim); margin-right: 0.5rem; }
      .stream .name { color: var(--accent); }
      .empty { color: var(--dim); font-style: italic; padding: 0.5rem 0; }
      .conn { font-size: 0.8rem; }
      .conn.on  { color: var(--good); }
      .conn.off { color: var(--bad); }
    </style>
  </head>
  <body>
    <div class="header">
      <div>
        <h1>Hypatia Watcher</h1>
        <div class="meta">
          Uptime <span id="uptime">—</span>
          · Dropped <span id="dropped">0</span>
          · <span id="conn" class="conn off">stream offline</span>
        </div>
      </div>
      <div class="meta" id="updated">never updated</div>
    </div>

    <div class="grid">
      <div class="card">
        <h2>Events / 5 minutes</h2>
        <table id="counts-m5"><tbody><tr><td class="empty">loading…</td></tr></tbody></table>
      </div>
      <div class="card">
        <h2>Events / 1 hour</h2>
        <table id="counts-h1"><tbody><tr><td class="empty">loading…</td></tr></tbody></table>
      </div>
      <div class="card">
        <h2>GenServer queue depths</h2>
        <table id="queues"><tbody><tr><td class="empty">loading…</td></tr></tbody></table>
      </div>
      <div class="card">
        <h2>Recipe health (actionable rows first)</h2>
        <table id="recipes"><tbody><tr><td class="empty">loading…</td></tr></tbody></table>
      </div>
    </div>

    <div class="card" style="margin-top:1rem">
      <h2>Recent alerts</h2>
      <div id="alerts" class="stream"><div class="empty">no alerts yet</div></div>
    </div>

    <div class="card" style="margin-top:1rem">
      <h2>Live telemetry stream</h2>
      <div id="stream" class="stream"><div class="empty">connecting…</div></div>
    </div>

    <script>
      const $ = (id) => document.getElementById(id);
      const fmt = {
        seconds(s) {
          if (s < 60) return s + "s";
          if (s < 3600) return Math.floor(s/60) + "m " + (s%60) + "s";
          return Math.floor(s/3600) + "h " + Math.floor((s%3600)/60) + "m";
        },
        time(ms) {
          const d = new Date(ms);
          return d.toISOString().slice(11, 19);
        },
        rate(r) {
          if (r === null || r === "insufficient_data") return "—";
          return Number(r).toFixed(2);
        }
      };

      function counts_rows(counts) {
        const entries = Object.entries(counts || {}).sort((a,b) => b[1] - a[1]);
        if (entries.length === 0) return '<tr><td class="empty">no events</td></tr>';
        return entries.map(([k,v]) =>
          `<tr><td>${k}</td><td class="num">${v}</td></tr>`
        ).join("");
      }

      function queue_rows(qs) {
        const entries = Object.entries(qs || {}).sort();
        if (entries.length === 0) return '<tr><td class="empty">supervisor not visible</td></tr>';
        return entries.map(([name, depth]) => {
          let cls = "good";
          if (depth === null) { depth = "—"; cls = "dim"; }
          else if (depth > 100) cls = "bad";
          else if (depth > 10) cls = "warn";
          return `<tr><td>${name}</td><td class="num ${cls}">${depth}</td></tr>`;
        }).join("");
      }

      function recipe_rows(rows) {
        if (!rows || rows.length === 0) return '<tr><td class="empty">no recipe outcomes yet</td></tr>';
        // Top 12 by ascending verification rate
        const top = rows.slice(0, 12);
        return `<tr><th>recipe</th><th class="num">dispatches</th><th class="num">rate</th><th>status</th></tr>` +
          top.map((r) => {
            const status = r.status;
            let cls = "";
            if (status === "quarantine_candidate") cls = "bad";
            else if (status === "degraded") cls = "warn";
            else if (status === "healthy") cls = "good";
            else cls = "dim";
            return `<tr>
              <td>${r.recipe_id}</td>
              <td class="num">${r.dispatches}</td>
              <td class="num">${fmt.rate(r.verification?.rate)}</td>
              <td><span class="pill ${cls}">${status}</span></td>
            </tr>`;
          }).join("");
      }

      async function pollStatus() {
        try {
          const res = await fetch("/api/status", { cache: "no-store" });
          if (!res.ok) throw new Error(res.status);
          const snap = await res.json();
          $("uptime").textContent = fmt.seconds(snap.uptime_seconds || 0);
          $("dropped").textContent = snap.dropped_events || 0;
          $("updated").textContent = "updated " + fmt.time(Date.now());
          $("counts-m5").querySelector("tbody").innerHTML = counts_rows(snap.counts?.m5);
          $("counts-h1").querySelector("tbody").innerHTML = counts_rows(snap.counts?.h1);
          $("queues").querySelector("tbody").innerHTML = queue_rows(snap.queue_depths);
        } catch (e) {
          $("updated").textContent = "status poll failed: " + e.message;
        }
      }

      async function pollRecipes() {
        try {
          const res = await fetch("/api/recipes", { cache: "no-store" });
          if (!res.ok) throw new Error(res.status);
          const body = await res.json();
          $("recipes").querySelector("tbody").innerHTML = recipe_rows(body.rows);
        } catch (e) { /* keep last */ }
      }

      async function pollAlerts() {
        try {
          const res = await fetch("/api/alerts", { cache: "no-store" });
          if (!res.ok) throw new Error(res.status);
          const body = await res.json();
          renderAlerts(body.rows || []);
        } catch (e) { /* keep last */ }
      }

      function renderAlerts(rows) {
        const el = $("alerts");
        if (!rows.length) {
          el.innerHTML = '<div class="empty">no alerts yet</div>';
          return;
        }
        el.innerHTML = rows.slice(0, 20).map((a) => {
          const sev = String(a.severity || "").toLowerCase();
          const pillClass = sev === "critical" || sev === "high"
            ? "bad" : sev === "medium" ? "warn" : "dim";
          const fed = a.metadata && a.metadata.federated_from
            ? ` <span class="pill dim">peer ${a.metadata.federated_from}</span>` : "";
          return `<div class="ev">
            <span class="ts">${fmt.time(a.at || Date.now())}</span>
            <span class="pill ${pillClass}">${a.severity || "?"}</span>
            <span class="name">${a.rule || "?"}</span>
            ${a.summary || ""}${fed}
          </div>`;
        }).join("");
      }

      function connectStream() {
        const es = new EventSource("/api/events");
        const stream = $("stream");
        const conn = $("conn");
        stream.innerHTML = "";
        conn.textContent = "stream live"; conn.className = "conn on";

        // EventSource fires the named 'event:' type, not 'message'.
        // Subscribe to each known event name dynamically: listen for
        // any incoming SSE event by attaching a generic listener via
        // the parent .onmessage AND also using addEventListener for
        // the typed events. Browsers route named events to typed
        // listeners only — so we capture from the EventSource directly.
        const known = [
          "hypatia.scan.complete",
          "hypatia.dispatch.decision",
          "hypatia.outcome.recorded",
          "hypatia.verification.result",
          "hypatia.quarantine.triggered",
          "hypatia.rate_limit.exceeded",
          "hypatia.neural.cycle",
          "hypatia.soundness.violation",
          "hypatia.anomaly.detected",
        ];

        known.forEach((name) => {
          es.addEventListener(name, (e) => appendEvent(name, e.data));
        });

        es.onerror = () => {
          conn.textContent = "stream offline (reconnecting…)";
          conn.className = "conn off";
        };
        es.onopen = () => {
          conn.textContent = "stream live"; conn.className = "conn on";
        };

        function appendEvent(name, raw) {
          let data = {};
          try { data = JSON.parse(raw); } catch (_) {}
          const div = document.createElement("div");
          div.className = "ev";
          const meta = data.metadata || {};
          const summary = Object.entries(meta).slice(0, 3).map(
            ([k,v]) => `${k}=${v}`
          ).join(" ");
          div.innerHTML = `<span class="ts">${fmt.time(data.at || Date.now())}</span>` +
            `<span class="name">${name}</span> ${summary}`;
          stream.insertBefore(div, stream.firstChild);
          // Cap the list so the DOM doesn't grow forever.
          while (stream.children.length > 80) stream.removeChild(stream.lastChild);
        }
      }

      // Boot
      pollStatus();
      pollRecipes();
      pollAlerts();
      connectStream();
      setInterval(pollStatus, 2000);
      setInterval(pollRecipes, 5000);
      setInterval(pollAlerts, 5000);
    </script>
  </body>
  </html>
  """

  @doc """
  Plug entry point for the dashboard HTML.
  """
  def call(conn, _opts) do
    conn
    |> put_resp_content_type("text/html; charset=utf-8")
    |> put_resp_header("cache-control", "no-store")
    |> send_resp(200, @html)
  end

  def init(opts), do: opts
end
