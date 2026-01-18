--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Views.Main_Menu - Implementation
-------------------------------------------------------------------------------

with Hypatia.Widgets; use Hypatia.Widgets;

package body Hypatia.Views.Main_Menu is

   use Hypatia.Widgets.List;
   use Hypatia.Widgets.Status_Bar;

   ---------------------------------------------------------------------------
   --  ASCII art logo
   ---------------------------------------------------------------------------

   Logo_Line_1 : constant String := "  _   _                       ";
   Logo_Line_2 : constant String := " | | | |_   _ _ __   ___ _ __ ";
   Logo_Line_3 : constant String := " | |_| | | | | '_ \ / _ \ '__|";
   Logo_Line_4 : constant String := " |  _  | |_| | |_) |  __/ |   ";
   Logo_Line_5 : constant String := " |_| |_|\__, | .__/ \___|_|   ";
   Logo_Line_6 : constant String := "        |___/|_|  CI/CD v" & Version;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------

   overriding procedure Initialize
      (V      : in out Main_Menu_View;
       Width  : Terminal_Width;
       Height : Terminal_Height)
   is
      Menu_Width  : constant Terminal_Width := 40;
      Menu_Height : constant Terminal_Height := 10;
      Menu_X      : constant Terminal_Width := (Width - Menu_Width) / 2;
      Menu_Y      : constant Terminal_Height := 10;
   begin
      V.Bounds := (Width, Height);

      --  Configure menu list
      V.Menu.Area := (Menu_X, Menu_Y, Menu_Width, Menu_Height);
      V.Menu.Show_Border := True;
      V.Menu.Show_Scrollbar := False;
      V.Menu.Focused := True;
      Set_Title (V.Menu, "Main Menu");

      --  Add menu items
      Clear (V.Menu);
      Add_Item (V.Menu, "  Scan Repository      ", Menu_Scan);
      Add_Item (V.Menu, "  Fleet Management     ", Menu_Fleet);
      Add_Item (V.Menu, "  Registry Browser     ", Menu_Registry);
      Add_Item (V.Menu, "  Settings             ", Menu_Settings);
      Add_Item (V.Menu, "  Help                 ", Menu_Help);
      Add_Item (V.Menu, "  Exit                 ", Menu_Exit);

      --  Configure status bar
      V.Status_Bar.Area := (1, Height, Width, 1);
      Set_Left_Text (V.Status_Bar, " HYPATIA ");
      Set_Menu_Hints (V.Status_Bar);
   end Initialize;

   ---------------------------------------------------------------------------
   --  Render
   ---------------------------------------------------------------------------

   overriding procedure Render
      (V    : Main_Menu_View;
       Term : in out Terminal_State)
   is
      Logo_X : constant Terminal_Width := (V.Bounds.Width - 30) / 2;
   begin
      --  Clear screen
      Clear_Screen (Term);

      --  Draw logo centered at top
      Move_Cursor (Term, (2, Logo_X));
      Put_Styled (Term, Logo_Line_1, Style_Bold);
      Move_Cursor (Term, (3, Logo_X));
      Put_Styled (Term, Logo_Line_2, Style_Bold);
      Move_Cursor (Term, (4, Logo_X));
      Put_Styled (Term, Logo_Line_3, Style_Bold);
      Move_Cursor (Term, (5, Logo_X));
      Put_Styled (Term, Logo_Line_4, Style_Bold);
      Move_Cursor (Term, (6, Logo_X));
      Put_Styled (Term, Logo_Line_5, Style_Bold);
      Move_Cursor (Term, (7, Logo_X));
      Put_Styled (Term, Logo_Line_6, Style_Muted);

      --  Draw tagline
      declare
         Tagline   : constant String := "Neurosymbolic CI/CD Intelligence Platform";
         Tagline_X : constant Terminal_Width := (V.Bounds.Width - Tagline'Length) / 2;
      begin
         Move_Cursor (Term, (9, Tagline_X));
         Put_Styled (Term, Tagline, Style_Muted);
      end;

      --  Draw menu
      Render (V.Menu, Term);

      --  Draw status bar
      Render (V.Status_Bar, Term);

      Flush (Term);
   end Render;

   ---------------------------------------------------------------------------
   --  Handle input
   ---------------------------------------------------------------------------

   overriding function Handle_Input
      (V     : in out Main_Menu_View;
       Event : Input_Event) return State_Machine.Navigation_Event
   is
      use State_Machine;
   begin
      --  Check for direct key commands
      case Event.Key is
         when Key_Char =>
            case Event.Char is
               when 'q' | 'Q' =>
                  return Event_Request_Exit;
               when '?' =>
                  return Event_Go_Help;
               when others =>
                  null;
            end case;

         when Key_Enter =>
            --  Handle menu selection
            declare
               Item : constant List_Item := Get_Selected_Item (V.Menu);
            begin
               case Item.Data is
                  when Menu_Scan =>
                     return Event_Go_Scan;
                  when Menu_Fleet =>
                     return Event_Go_Fleet;
                  when Menu_Registry =>
                     return Event_Go_Registry;
                  when Menu_Settings =>
                     return Event_Go_Settings;
                  when Menu_Help =>
                     return Event_Go_Help;
                  when Menu_Exit =>
                     return Event_Request_Exit;
                  when others =>
                     null;
               end case;
            end;

         when Key_Escape =>
            return Event_Request_Exit;

         when others =>
            null;
      end case;

      --  Let menu handle navigation keys
      if Handle_Input (V.Menu, Event) then
         null;  -- Menu consumed the input
      end if;

      return Event_None;
   end Handle_Input;

   ---------------------------------------------------------------------------
   --  On Enter
   ---------------------------------------------------------------------------

   overriding procedure On_Enter (V : in out Main_Menu_View) is
   begin
      Set_Focus (V.Menu, True);
      Set_Menu_Hints (V.Status_Bar);
   end On_Enter;

end Hypatia.Views.Main_Menu;
