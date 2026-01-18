--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Views.Help - Implementation
-------------------------------------------------------------------------------

with Hypatia.Widgets; use Hypatia.Widgets;

package body Hypatia.Views.Help is

   use Hypatia.Widgets.List;
   use Hypatia.Widgets.Status_Bar;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------

   overriding procedure Initialize
      (V      : in out Help_View;
       Width  : Terminal_Width;
       Height : Terminal_Height)
   is
      List_Height : constant Terminal_Height := Height - 5;
   begin
      V.Bounds := (Width, Height);

      --  Configure help list
      V.Help_List.Area := (2, 3, Width - 4, List_Height);
      V.Help_List.Show_Border := True;
      V.Help_List.Show_Scrollbar := True;
      V.Help_List.Focused := True;
      Set_Title (V.Help_List, "Help");

      --  Configure status bar
      V.Status_Bar.Area := (1, Height, Width, 1);
      Set_Left_Text (V.Status_Bar, " HELP ");

      --  Load help content
      Load_Help_Content (V);
   end Initialize;

   ---------------------------------------------------------------------------
   --  Render
   ---------------------------------------------------------------------------

   overriding procedure Render
      (V    : Help_View;
       Term : in out Terminal_State)
   is
   begin
      Clear_Screen (Term);

      --  Draw title
      Move_Cursor (Term, (1, 2));
      Put_Styled (Term, "Hypatia TUI - Help", Style_Bold);

      --  Draw help list
      Render (V.Help_List, Term);

      --  Draw status bar
      Render (V.Status_Bar, Term);

      Flush (Term);
   end Render;

   ---------------------------------------------------------------------------
   --  Handle input
   ---------------------------------------------------------------------------

   overriding function Handle_Input
      (V     : in out Help_View;
       Event : Input_Event) return State_Machine.Navigation_Event
   is
      use State_Machine;
   begin
      case Event.Key is
         when Key_Escape | Key_Enter =>
            return Event_Go_Back;

         when Key_Char =>
            case Event.Char is
               when 'q' | 'Q' =>
                  return Event_Go_Back;
               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;

      --  Let list handle navigation for scrolling
      if Handle_Input (V.Help_List, Event) then
         null;
      end if;

      return Event_None;
   end Handle_Input;

   ---------------------------------------------------------------------------
   --  On Enter
   ---------------------------------------------------------------------------

   overriding procedure On_Enter (V : in out Help_View) is
   begin
      Set_Focus (V.Help_List, True);
      Clear_Hints (V.Status_Bar);
      Add_Hint (V.Status_Bar, "↑↓", "Scroll");
      Add_Hint (V.Status_Bar, "Esc", "Back");
   end On_Enter;

   ---------------------------------------------------------------------------
   --  Load help content
   ---------------------------------------------------------------------------

   procedure Load_Help_Content (V : in out Help_View) is
   begin
      Clear (V.Help_List);

      Add_Item (V.Help_List, "  HYPATIA - Neurosymbolic CI/CD Intelligence Platform");
      Add_Item (V.Help_List, "  Version " & Version);
      Add_Item (V.Help_List, "");
      Add_Item (V.Help_List, "  ═══════════════════════════════════════════════════════");
      Add_Item (V.Help_List, "  GLOBAL KEYBOARD SHORTCUTS");
      Add_Item (V.Help_List, "  ═══════════════════════════════════════════════════════");
      Add_Item (V.Help_List, "");
      Add_Item (V.Help_List, "    q, Q           Quit / Go back");
      Add_Item (V.Help_List, "    ?              Show this help");
      Add_Item (V.Help_List, "    Esc            Go back / Cancel");
      Add_Item (V.Help_List, "");
      Add_Item (V.Help_List, "  ═══════════════════════════════════════════════════════");
      Add_Item (V.Help_List, "  NAVIGATION");
      Add_Item (V.Help_List, "  ═══════════════════════════════════════════════════════");
      Add_Item (V.Help_List, "");
      Add_Item (V.Help_List, "    ↑, k           Move up");
      Add_Item (V.Help_List, "    ↓, j           Move down");
      Add_Item (V.Help_List, "    PgUp           Page up");
      Add_Item (V.Help_List, "    PgDn           Page down");
      Add_Item (V.Help_List, "    Home           Go to first item");
      Add_Item (V.Help_List, "    End            Go to last item");
      Add_Item (V.Help_List, "    Enter          Select / Confirm");
      Add_Item (V.Help_List, "");
      Add_Item (V.Help_List, "  ═══════════════════════════════════════════════════════");
      Add_Item (V.Help_List, "  SCAN VIEW");
      Add_Item (V.Help_List, "  ═══════════════════════════════════════════════════════");
      Add_Item (V.Help_List, "");
      Add_Item (V.Help_List, "    s, S           Start/restart scan");
      Add_Item (V.Help_List, "    f, F           Apply auto-fix");
      Add_Item (V.Help_List, "    Enter          View issue details");
      Add_Item (V.Help_List, "");
      Add_Item (V.Help_List, "  ═══════════════════════════════════════════════════════");
      Add_Item (V.Help_List, "  FLEET VIEW");
      Add_Item (V.Help_List, "  ═══════════════════════════════════════════════════════");
      Add_Item (V.Help_List, "");
      Add_Item (V.Help_List, "    a, A           Add repository");
      Add_Item (V.Help_List, "    r, R           Refresh status");
      Add_Item (V.Help_List, "    s, S           Scan selected repo");
      Add_Item (V.Help_List, "    Enter          Open repository");
      Add_Item (V.Help_List, "");
      Add_Item (V.Help_List, "  ═══════════════════════════════════════════════════════");
      Add_Item (V.Help_List, "  REGISTRY VIEW");
      Add_Item (V.Help_List, "  ═══════════════════════════════════════════════════════");
      Add_Item (V.Help_List, "");
      Add_Item (V.Help_List, "    /              Search rules");
      Add_Item (V.Help_List, "    r, R           Refresh registry");
      Add_Item (V.Help_List, "    Space          Toggle rule");
      Add_Item (V.Help_List, "    Enter          View rule details");
      Add_Item (V.Help_List, "");
      Add_Item (V.Help_List, "  ═══════════════════════════════════════════════════════");
      Add_Item (V.Help_List, "  ABOUT");
      Add_Item (V.Help_List, "  ═══════════════════════════════════════════════════════");
      Add_Item (V.Help_List, "");
      Add_Item (V.Help_List, "    License:       PLMP-1.0-or-later");
      Add_Item (V.Help_List, "    Homepage:      https://github.com/hyperpolymath/cicd-hyper-a");
      Add_Item (V.Help_List, "    Documentation: hyper --help");
      Add_Item (V.Help_List, "");
   end Load_Help_Content;

end Hypatia.Views.Help;
