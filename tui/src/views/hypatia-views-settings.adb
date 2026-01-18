--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Views.Settings - Implementation
-------------------------------------------------------------------------------

with Hypatia.Widgets; use Hypatia.Widgets;

package body Hypatia.Views.Settings is

   use Hypatia.Widgets.List;
   use Hypatia.Widgets.Status_Bar;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------

   overriding procedure Initialize
      (V      : in out Settings_View;
       Width  : Terminal_Width;
       Height : Terminal_Height)
   is
      List_Height : constant Terminal_Height := Height - 5;
   begin
      V.Bounds := (Width, Height);

      --  Configure settings list
      V.Settings_List.Area := (2, 3, Width - 4, List_Height);
      V.Settings_List.Show_Border := True;
      V.Settings_List.Show_Scrollbar := True;
      V.Settings_List.Focused := True;
      Set_Title (V.Settings_List, "Settings");

      --  Configure status bar
      V.Status_Bar.Area := (1, Height, Width, 1);
      Set_Left_Text (V.Status_Bar, " SETTINGS ");

      --  Load initial settings
      Refresh_Settings (V);
   end Initialize;

   ---------------------------------------------------------------------------
   --  Render
   ---------------------------------------------------------------------------

   overriding procedure Render
      (V    : Settings_View;
       Term : in out Terminal_State)
   is
   begin
      Clear_Screen (Term);

      --  Draw title
      Move_Cursor (Term, (1, 2));
      Put_Styled (Term, "Configuration", Style_Bold);

      --  Draw settings list
      Render (V.Settings_List, Term);

      --  Draw status bar
      Render (V.Status_Bar, Term);

      Flush (Term);
   end Render;

   ---------------------------------------------------------------------------
   --  Handle input
   ---------------------------------------------------------------------------

   overriding function Handle_Input
      (V     : in out Settings_View;
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
               when '?' =>
                  return Event_Go_Help;
               when others =>
                  null;
            end case;

         when Key_Enter =>
            --  Toggle/edit selected setting (simplified)
            return Event_None;

         when others =>
            null;
      end case;

      --  Let list handle navigation
      if Handle_Input (V.Settings_List, Event) then
         null;
      end if;

      return Event_None;
   end Handle_Input;

   ---------------------------------------------------------------------------
   --  On Enter
   ---------------------------------------------------------------------------

   overriding procedure On_Enter (V : in out Settings_View) is
   begin
      Set_Focus (V.Settings_List, True);
      Clear_Hints (V.Status_Bar);
      Add_Hint (V.Status_Bar, "↑↓", "Navigate");
      Add_Hint (V.Status_Bar, "Enter", "Edit");
      Add_Hint (V.Status_Bar, "Esc", "Back");
   end On_Enter;

   ---------------------------------------------------------------------------
   --  Refresh settings
   ---------------------------------------------------------------------------

   procedure Refresh_Settings (V : in out Settings_View) is
   begin
      Clear (V.Settings_List);

      --  Display settings
      Add_Item (V.Settings_List, "  Output Format          [json]");
      Add_Item (V.Settings_List, "  Color Theme            [dark]");
      Add_Item (V.Settings_List, "  Auto-fix Enabled       [yes]");
      Add_Item (V.Settings_List, "  Parallel Scans         [4]");
      Add_Item (V.Settings_List, "  Cache Directory        [~/.cache/hyper]");
      Add_Item (V.Settings_List, "  Log Level              [info]");
      Add_Item (V.Settings_List, "  ─────────────────────────────────────");
      Add_Item (V.Settings_List, "  Registry URL           [https://registry.hyper.io]");
      Add_Item (V.Settings_List, "  Registry Cache TTL     [1h]");
      Add_Item (V.Settings_List, "  ─────────────────────────────────────");
      Add_Item (V.Settings_List, "  GitHub Token           [configured]");
      Add_Item (V.Settings_List, "  GitLab Token           [not set]");
      Add_Item (V.Settings_List, "  Bitbucket Token        [not set]");
      Add_Item (V.Settings_List, "  ─────────────────────────────────────");
      Add_Item (V.Settings_List, "  Reset to Defaults");
      Add_Item (V.Settings_List, "  Export Configuration");
      Add_Item (V.Settings_List, "  Import Configuration");
   end Refresh_Settings;

end Hypatia.Views.Settings;
