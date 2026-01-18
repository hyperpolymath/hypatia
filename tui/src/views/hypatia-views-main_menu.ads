--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Views.Main_Menu - Main menu view
--
--  The central hub for navigating to different sections of the TUI.
-------------------------------------------------------------------------------

with Hypatia.Widgets.List;
with Hypatia.Widgets.Status_Bar;

package Hypatia.Views.Main_Menu is

   --  Menu options (mapped to Data field in List_Item)
   Menu_Scan     : constant := 1;
   Menu_Fleet    : constant := 2;
   Menu_Registry : constant := 3;
   Menu_Settings : constant := 4;
   Menu_Help     : constant := 5;
   Menu_Exit     : constant := 6;

   --  Main menu view
   type Main_Menu_View is new View with record
      Menu       : Widgets.List.List_Widget;
      Status_Bar : Widgets.Status_Bar.Status_Bar_Widget;
   end record;

   --  Initialize the view
   overriding procedure Initialize
      (V      : in out Main_Menu_View;
       Width  : Terminal_Width;
       Height : Terminal_Height);

   --  Render the view
   overriding procedure Render
      (V    : Main_Menu_View;
       Term : in out Terminal_State);

   --  Handle input
   overriding function Handle_Input
      (V     : in out Main_Menu_View;
       Event : Input_Event) return State_Machine.Navigation_Event;

   --  Called when view becomes active
   overriding procedure On_Enter (V : in out Main_Menu_View);

end Hypatia.Views.Main_Menu;
