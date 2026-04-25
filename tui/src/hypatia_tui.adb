-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Hypatia Ada TUI — terminal dashboard driven by Elixir Port.
--
-- Protocol: parent sends newline-terminated JSON snapshot lines on stdin.
-- Each snapshot is a flat JSON object (no nesting required for parsing):
--
--   {"repos":302,"weak_points":3385,"dispatched":1635,"outcomes":16671,
--    "recipes":46,"confidence":0.91,"status":"active"}
--
-- The TUI renders each snapshot as a live dashboard and waits for the next.
-- A line containing only "EXIT" signals clean shutdown.
--
-- ANSI escape codes are used for colouring; no external dependencies.

with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings;          use Ada.Strings;
with Ada.Strings.Fixed;    use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Hypatia_TUI is

   -- ── ANSI helpers ──────────────────────────────────────────────────────

   ESC    : constant String := ASCII.ESC & "[";
   RESET  : constant String := ESC & "0m";
   BOLD   : constant String := ESC & "1m";
   GREEN  : constant String := ESC & "32m";
   YELLOW : constant String := ESC & "33m";
   CYAN   : constant String := ESC & "36m";
   RED    : constant String := ESC & "31m";
   CLEAR  : constant String := ESC & "2J" & ESC & "H";

   -- ── Snapshot fields (parsed from a single JSON line) ──────────────────

   type Snapshot is record
      Repos       : Natural := 0;
      Weak_Points : Natural := 0;
      Dispatched  : Natural := 0;
      Outcomes    : Natural := 0;
      Recipes     : Natural := 0;
      Confidence  : Float   := 0.0;
      Status      : Unbounded_String := To_Unbounded_String ("unknown");
   end record;

   -- ── Minimal JSON field extractor ─────────────────────────────────────
   -- Extracts the value of `"key":value` from a flat JSON string.
   -- Returns empty string when key is absent.

   function Extract_String
     (Json : String; Key : String) return String
   is
      Search : constant String := """" & Key & """:""";
      Pos    : Natural;
   begin
      Pos := Index (Json, Search);
      if Pos = 0 then
         return "";
      end if;
      declare
         Start : constant Natural := Pos + Search'Length;
         Stop  : Natural := Start;
      begin
         while Stop <= Json'Last and then Json (Stop) /= '"' loop
            Stop := Stop + 1;
         end loop;
         return Json (Start .. Stop - 1);
      end;
   end Extract_String;

   function Extract_Integer
     (Json : String; Key : String) return Natural
   is
      Search : constant String := """" & Key & """:";
      Pos    : Natural;
   begin
      Pos := Index (Json, Search);
      if Pos = 0 then
         return 0;
      end if;
      declare
         Start : constant Natural := Pos + Search'Length;
         Stop  : Natural := Start;
         Value : Natural := 0;
      begin
         while Stop <= Json'Last
           and then Json (Stop) in '0' .. '9'
         loop
            Value := Value * 10
              + (Character'Pos (Json (Stop)) - Character'Pos ('0'));
            Stop := Stop + 1;
         end loop;
         return Value;
      end;
   end Extract_Integer;

   function Extract_Float
     (Json : String; Key : String) return Float
   is
      Search : constant String := """" & Key & """:";
      Pos    : Natural;
   begin
      Pos := Index (Json, Search);
      if Pos = 0 then
         return 0.0;
      end if;
      declare
         Start : Natural := Pos + Search'Length;
         Stop  : Natural := Start;
      begin
         while Stop <= Json'Last
           and then (Json (Stop) in '0' .. '9' or else Json (Stop) = '.')
         loop
            Stop := Stop + 1;
         end loop;
         return Float'Value (Json (Start .. Stop - 1));
      exception
         when others => return 0.0;
      end;
   end Extract_Float;

   -- ── Parse a single JSON line into a Snapshot ─────────────────────────

   function Parse (Line : String) return Snapshot is
      S : Snapshot;
   begin
      S.Repos       := Extract_Integer (Line, "repos");
      S.Weak_Points := Extract_Integer (Line, "weak_points");
      S.Dispatched  := Extract_Integer (Line, "dispatched");
      S.Outcomes    := Extract_Integer (Line, "outcomes");
      S.Recipes     := Extract_Integer (Line, "recipes");
      S.Confidence  := Extract_Float   (Line, "confidence");
      S.Status      := To_Unbounded_String (Extract_String (Line, "status"));
      return S;
   end Parse;

   -- ── Render a snapshot to the terminal ────────────────────────────────

   procedure Render (S : Snapshot) is
      Conf_Colour : constant String :=
        (if S.Confidence >= 0.95 then GREEN
         elsif S.Confidence >= 0.85 then YELLOW
         else RED);
      Status_Str  : constant String := To_String (S.Status);
   begin
      Put (CLEAR);
      Put_Line (BOLD & CYAN & "╔══════════════════════════════════════╗" & RESET);
      Put_Line (BOLD & CYAN & "║   Hypatia — Neurosymbolic CI/CD      ║" & RESET);
      Put_Line (BOLD & CYAN & "╠══════════════════════════════════════╣" & RESET);

      Put (CYAN & "║  Status      " & RESET);
      Put (BOLD);
      if Status_Str = "active" then Put (GREEN); else Put (YELLOW); end if;
      Put_Line (Status_Str & RESET & CYAN & "                ║" & RESET);

      Put_Line (CYAN & "╠══════════════════════════════════════╣" & RESET);

      Put (CYAN & "║  Repos scanned   " & RESET);
      Ada.Integer_Text_IO.Put (S.Repos, Width => 6);
      Put_Line (CYAN & "               ║" & RESET);

      Put (CYAN & "║  Weak points     " & RESET);
      Ada.Integer_Text_IO.Put (S.Weak_Points, Width => 6);
      Put_Line (CYAN & "               ║" & RESET);

      Put (CYAN & "║  Dispatched      " & RESET);
      Ada.Integer_Text_IO.Put (S.Dispatched, Width => 6);
      Put_Line (CYAN & "               ║" & RESET);

      Put (CYAN & "║  Outcomes        " & RESET);
      Ada.Integer_Text_IO.Put (S.Outcomes, Width => 6);
      Put_Line (CYAN & "               ║" & RESET);

      Put (CYAN & "║  Recipes         " & RESET);
      Ada.Integer_Text_IO.Put (S.Recipes, Width => 6);
      Put_Line (CYAN & "               ║" & RESET);

      Put (CYAN & "╠══════════════════════════════════════╣" & RESET);
      Put (CYAN & "║  Confidence  " & RESET & Conf_Colour & BOLD);
      declare
         Pct : constant Natural := Natural (S.Confidence * 100.0);
      begin
         Ada.Integer_Text_IO.Put (Pct, Width => 3);
         Put ("%" & RESET & CYAN & "                    ║" & RESET);
         New_Line;
      end;
      Put_Line (CYAN & "╚══════════════════════════════════════╝" & RESET);
      Put_Line (BOLD & "  (waiting for next snapshot…)" & RESET);
   end Render;

   -- ── Main loop ────────────────────────────────────────────────────────

   Line : Unbounded_String;

begin
   loop
      begin
         Line := To_Unbounded_String (Get_Line);
      exception
         when End_Error => exit;
      end;

      declare
         L : constant String := To_String (Line);
      begin
         exit when L = "EXIT";
         if L'Length > 2 and then L (L'First) = '{' then
            Render (Parse (L));
         end if;
      end;
   end loop;

   Put (CLEAR);
   Put_Line (BOLD & "Hypatia TUI closed." & RESET);
end Hypatia_TUI;
