--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Views.Scan - Implementation
-------------------------------------------------------------------------------

with Hypatia.Widgets; use Hypatia.Widgets;

package body Hypatia.Views.Scan is

   use Hypatia.Widgets.List;
   use Hypatia.Widgets.Status_Bar;

   ---------------------------------------------------------------------------
   --  Forward declarations
   ---------------------------------------------------------------------------

   function Severity_Prefix (S : Severity_Level) return String;

   ---------------------------------------------------------------------------
   --  Severity prefix for display
   ---------------------------------------------------------------------------

   function Severity_Prefix (S : Severity_Level) return String is
   begin
      case S is
         when Sev_Info     => return "[INFO] ";
         when Sev_Warning  => return "[WARN] ";
         when Sev_Error    => return "[ERR]  ";
         when Sev_Critical => return "[CRIT] ";
      end case;
   end Severity_Prefix;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------

   overriding procedure Initialize
      (V      : in out Scan_View;
       Width  : Terminal_Width;
       Height : Terminal_Height)
   is
      List_Height : constant Terminal_Height := Height - 6;
   begin
      V.Bounds := (Width, Height);

      --  Configure results list
      V.Results_List.Area := (2, 4, Width - 4, List_Height);
      V.Results_List.Show_Border := True;
      V.Results_List.Show_Scrollbar := True;
      V.Results_List.Focused := True;
      V.Results_List.Empty_Message := To_Unbounded_String ("No scan results. Press 's' to start scan.");
      Set_Title (V.Results_List, "Scan Results");

      --  Configure status bar
      V.Status_Bar.Area := (1, Height, Width, 1);
      Set_Left_Text (V.Status_Bar, " SCAN ");
      Set_List_Hints (V.Status_Bar);
   end Initialize;

   ---------------------------------------------------------------------------
   --  Render
   ---------------------------------------------------------------------------

   overriding procedure Render
      (V    : Scan_View;
       Term : in out Terminal_State)
   is
   begin
      Clear_Screen (Term);

      --  Draw title bar
      Move_Cursor (Term, (1, 2));
      Put_Styled (Term, "Repository Scan - ", Style_Bold);
      if Length (V.Repo_Path) > 0 then
         Put_Styled (Term, To_String (V.Repo_Path), Style_Normal);
      else
         Put_Styled (Term, "(no repository)", Style_Muted);
      end if;

      --  Draw summary line
      Move_Cursor (Term, (2, 2));
      Put_Styled (Term, "Issues: ", Style_Normal);
      Put_Styled (Term, Natural'Image (V.Total_Issues), Style_Bold);

      Put_Styled (Term, "  Critical: ", Style_Normal);
      if V.Criticals > 0 then
         Put_Styled (Term, Natural'Image (V.Criticals), Style_Error);
      else
         Put_Styled (Term, " 0", Style_Success);
      end if;

      Put_Styled (Term, "  Errors: ", Style_Normal);
      if V.Errors > 0 then
         Put_Styled (Term, Natural'Image (V.Errors), Style_Error);
      else
         Put_Styled (Term, " 0", Style_Success);
      end if;

      Put_Styled (Term, "  Warnings: ", Style_Normal);
      if V.Warnings > 0 then
         Put_Styled (Term, Natural'Image (V.Warnings), Style_Warning);
      else
         Put_Styled (Term, " 0", Style_Success);
      end if;

      --  Draw results list
      Render (V.Results_List, Term);

      --  Draw status bar
      Render (V.Status_Bar, Term);

      Flush (Term);
   end Render;

   ---------------------------------------------------------------------------
   --  Handle input
   ---------------------------------------------------------------------------

   overriding function Handle_Input
      (V     : in out Scan_View;
       Event : Input_Event) return State_Machine.Navigation_Event
   is
      use State_Machine;
   begin
      case Event.Key is
         when Key_Escape =>
            return Event_Go_Back;

         when Key_Char =>
            case Event.Char is
               when 'q' | 'Q' =>
                  return Event_Go_Back;
               when 's' | 'S' =>
                  --  Start/restart scan (demo)
                  Load_Demo_Results (V);
                  return Event_None;
               when '?' =>
                  return Event_Go_Help;
               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;

      --  Let list handle navigation keys
      if Handle_Input (V.Results_List, Event) then
         null;
      end if;

      return Event_None;
   end Handle_Input;

   ---------------------------------------------------------------------------
   --  On Enter
   ---------------------------------------------------------------------------

   overriding procedure On_Enter (V : in out Scan_View) is
   begin
      Set_Focus (V.Results_List, True);
      Set_List_Hints (V.Status_Bar);
      Add_Hint (V.Status_Bar, "s", "Scan");
   end On_Enter;

   ---------------------------------------------------------------------------
   --  Add a scan result
   ---------------------------------------------------------------------------

   procedure Add_Result
      (V        : in out Scan_View;
       Rule_ID  : String;
       Message  : String;
       Severity : Severity_Level;
       File     : String := "";
       Line     : Natural := 0)
   is
      Display : Unbounded_String;
   begin
      Append (Display, Severity_Prefix (Severity));
      Append (Display, "[");
      Append (Display, Rule_ID);
      Append (Display, "] ");
      Append (Display, Message);

      if File'Length > 0 then
         Append (Display, " (");
         Append (Display, File);
         if Line > 0 then
            Append (Display, ":");
            Append (Display, Natural'Image (Line));
         end if;
         Append (Display, ")");
      end if;

      Add_Item (V.Results_List, To_String (Display), Severity_Level'Pos (Severity));

      V.Total_Issues := V.Total_Issues + 1;
      case Severity is
         when Sev_Critical => V.Criticals := V.Criticals + 1;
         when Sev_Error    => V.Errors := V.Errors + 1;
         when Sev_Warning  => V.Warnings := V.Warnings + 1;
         when Sev_Info     => null;
      end case;
   end Add_Result;

   ---------------------------------------------------------------------------
   --  Clear all results
   ---------------------------------------------------------------------------

   procedure Clear_Results (V : in out Scan_View) is
   begin
      Clear (V.Results_List);
      V.Total_Issues := 0;
      V.Criticals := 0;
      V.Errors := 0;
      V.Warnings := 0;
   end Clear_Results;

   ---------------------------------------------------------------------------
   --  Set repository path
   ---------------------------------------------------------------------------

   procedure Set_Repo_Path (V : in out Scan_View; Path : String) is
   begin
      V.Repo_Path := To_Unbounded_String (Path);
   end Set_Repo_Path;

   ---------------------------------------------------------------------------
   --  Load demo results
   ---------------------------------------------------------------------------

   procedure Load_Demo_Results (V : in out Scan_View) is
   begin
      Clear_Results (V);
      Set_Repo_Path (V, "/var/mnt/eclipse/repos/cicd-hyper-a");

      Add_Result (V, "SEC-001", "Unpinned GitHub Action detected", Sev_Critical,
                  ".github/workflows/ci.yml", 15);
      Add_Result (V, "SEC-002", "Missing workflow permissions declaration", Sev_Error,
                  ".github/workflows/release.yml", 1);
      Add_Result (V, "LINT-001", "Missing SPDX license header", Sev_Warning,
                  "src/main.rs", 1);
      Add_Result (V, "DEP-001", "Outdated dependency: serde 1.0.100 -> 1.0.200", Sev_Warning,
                  "Cargo.toml", 12);
      Add_Result (V, "CFG-001", "CodeQL language mismatch", Sev_Error,
                  ".github/workflows/codeql.yml", 25);
      Add_Result (V, "DOC-001", "README.md missing required sections", Sev_Info,
                  "README.md", 0);
      Add_Result (V, "SEC-003", "Secret potentially exposed in logs", Sev_Critical,
                  ".github/workflows/deploy.yml", 42);
      Add_Result (V, "LINT-002", "Trailing whitespace", Sev_Info,
                  "src/lib.rs", 156);
   end Load_Demo_Results;

end Hypatia.Views.Scan;
