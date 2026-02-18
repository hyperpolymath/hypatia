--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Views.Registry - Implementation
-------------------------------------------------------------------------------

with Hypatia.Widgets; use Hypatia.Widgets;

package body Hypatia.Views.Registry is

   use Hypatia.Widgets.List;
   use Hypatia.Widgets.Status_Bar;

   ---------------------------------------------------------------------------
   --  Forward declarations
   ---------------------------------------------------------------------------

   function Category_Tag (C : Rule_Category) return String;

   ---------------------------------------------------------------------------
   --  Category tag
   ---------------------------------------------------------------------------

   function Category_Tag (C : Rule_Category) return String is
   begin
      case C is
         when Cat_Security    => return "[SEC] ";
         when Cat_Quality     => return "[QUA] ";
         when Cat_Style       => return "[STY] ";
         when Cat_Performance => return "[PRF] ";
         when Cat_Compliance  => return "[CMP] ";
      end case;
   end Category_Tag;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------

   overriding procedure Initialize
      (V      : in out Registry_View;
       Width  : Terminal_Width;
       Height : Terminal_Height)
   is
      List_Height : constant Terminal_Height := Height - 6;
   begin
      V.Bounds := (Width, Height);

      --  Configure rules list
      V.Rules_List.Area := (2, 4, Width - 4, List_Height);
      V.Rules_List.Show_Border := True;
      V.Rules_List.Show_Scrollbar := True;
      V.Rules_List.Focused := True;
      V.Rules_List.Empty_Message := To_Unbounded_String ("No rules loaded. Press 'r' to refresh.");
      Set_Title (V.Rules_List, "Rule Registry");

      --  Configure status bar
      V.Status_Bar.Area := (1, Height, Width, 1);
      Set_Left_Text (V.Status_Bar, " REGISTRY ");
      Set_List_Hints (V.Status_Bar);
   end Initialize;

   ---------------------------------------------------------------------------
   --  Render
   ---------------------------------------------------------------------------

   overriding procedure Render
      (V    : Registry_View;
       Term : in out Terminal_State)
   is
   begin
      Clear_Screen (Term);

      --  Draw title
      Move_Cursor (Term, (1, 2));
      Put_Styled (Term, "Rule Registry", Style_Bold);

      --  Draw summary
      Move_Cursor (Term, (2, 2));
      Put_Styled (Term, "Total Rules: ", Style_Normal);
      Put_Styled (Term, Natural'Image (V.Total_Rules), Style_Bold);
      Put_Styled (Term, "  Active: ", Style_Normal);
      Put_Styled (Term, Natural'Image (V.Active_Rules), Style_Success);
      Put_Styled (Term, "  Disabled: ", Style_Normal);
      Put_Styled (Term, Natural'Image (V.Total_Rules - V.Active_Rules), Style_Muted);

      --  Draw filter if set
      if Length (V.Filter_Text) > 0 then
         Put_Styled (Term, "  Filter: ", Style_Normal);
         Put_Styled (Term, To_String (V.Filter_Text), Style_Info);
      end if;

      --  Draw rules list
      Render (V.Rules_List, Term);

      --  Draw status bar
      Render (V.Status_Bar, Term);

      Flush (Term);
   end Render;

   ---------------------------------------------------------------------------
   --  Handle input
   ---------------------------------------------------------------------------

   overriding function Handle_Input
      (V     : in out Registry_View;
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
               when 'r' | 'R' =>
                  --  Refresh rules
                  Load_Demo_Rules (V);
                  return Event_None;
               when '/' =>
                  --  Start filter/search (simplified for demo)
                  return Event_None;
               when '?' =>
                  return Event_Go_Help;
               when others =>
                  null;
            end case;

         when Key_Enter =>
            --  View rule details (could open detail view)
            return Event_None;

         when others =>
            null;
      end case;

      --  Let list handle navigation
      if Handle_Input (V.Rules_List, Event) then
         null;
      end if;

      return Event_None;
   end Handle_Input;

   ---------------------------------------------------------------------------
   --  On Enter
   ---------------------------------------------------------------------------

   overriding procedure On_Enter (V : in out Registry_View) is
   begin
      Set_Focus (V.Rules_List, True);
      Clear_Hints (V.Status_Bar);
      Add_Hint (V.Status_Bar, "↑↓", "Navigate");
      Add_Hint (V.Status_Bar, "/", "Search");
      Add_Hint (V.Status_Bar, "r", "Refresh");
      Add_Hint (V.Status_Bar, "Esc", "Back");
   end On_Enter;

   ---------------------------------------------------------------------------
   --  Add a rule
   ---------------------------------------------------------------------------

   procedure Add_Rule
      (V           : in out Registry_View;
       Rule_ID     : String;
       Name        : String;
       Category    : Rule_Category;
       Enabled     : Boolean := True)
   is
      Display : Unbounded_String;
      Status  : constant String := (if Enabled then "[✓] " else "[ ] ");
   begin
      Append (Display, Status);
      Append (Display, Category_Tag (Category));
      Append (Display, Rule_ID);
      Append (Display, " - ");
      Append (Display, Name);

      Add_Item (V.Rules_List, To_String (Display), Rule_Category'Pos (Category), Enabled);

      V.Total_Rules := V.Total_Rules + 1;
      if Enabled then
         V.Active_Rules := V.Active_Rules + 1;
      end if;
   end Add_Rule;

   ---------------------------------------------------------------------------
   --  Clear all rules
   ---------------------------------------------------------------------------

   procedure Clear_Rules (V : in out Registry_View) is
   begin
      Clear (V.Rules_List);
      V.Total_Rules := 0;
      V.Active_Rules := 0;
   end Clear_Rules;

   ---------------------------------------------------------------------------
   --  Load demo rules
   ---------------------------------------------------------------------------

   procedure Load_Demo_Rules (V : in out Registry_View) is
   begin
      Clear_Rules (V);

      --  Security rules
      Add_Rule (V, "SEC-001", "Pinned GitHub Actions", Cat_Security);
      Add_Rule (V, "SEC-002", "Workflow Permissions", Cat_Security);
      Add_Rule (V, "SEC-003", "Secret Scanning", Cat_Security);
      Add_Rule (V, "SEC-004", "Dependency Review", Cat_Security);
      Add_Rule (V, "SEC-005", "CodeQL Analysis", Cat_Security);

      --  Quality rules
      Add_Rule (V, "QUA-001", "Test Coverage", Cat_Quality);
      Add_Rule (V, "QUA-002", "Documentation", Cat_Quality, False);
      Add_Rule (V, "QUA-003", "Code Review Required", Cat_Quality);

      --  Style rules
      Add_Rule (V, "STY-001", "SPDX License Headers", Cat_Style);
      Add_Rule (V, "STY-002", "EditorConfig Compliance", Cat_Style);
      Add_Rule (V, "STY-003", "Trailing Whitespace", Cat_Style, False);

      --  Performance rules
      Add_Rule (V, "PRF-001", "Workflow Caching", Cat_Performance);
      Add_Rule (V, "PRF-002", "Artifact Retention", Cat_Performance);

      --  Compliance rules
      Add_Rule (V, "CMP-001", "OpenSSF Scorecard", Cat_Compliance);
      Add_Rule (V, "CMP-002", "SLSA Provenance", Cat_Compliance);
      Add_Rule (V, "CMP-003", "Branch Protection", Cat_Compliance);
   end Load_Demo_Rules;

end Hypatia.Views.Registry;
