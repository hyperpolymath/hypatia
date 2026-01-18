--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Views.Help - Help view
--
--  Display help information and keyboard shortcuts.
-------------------------------------------------------------------------------

with Hypatia.Widgets.List;
with Hypatia.Widgets.Status_Bar;

package Hypatia.Views.Help is

   --  Help view
   type Help_View is new View with record
      Help_List  : Widgets.List.List_Widget;
      Status_Bar : Widgets.Status_Bar.Status_Bar_Widget;
   end record;

   --  Initialize the view
   overriding procedure Initialize
      (V      : in out Help_View;
       Width  : Terminal_Width;
       Height : Terminal_Height);

   --  Render the view
   overriding procedure Render
      (V    : Help_View;
       Term : in out Terminal_State);

   --  Handle input
   overriding function Handle_Input
      (V     : in out Help_View;
       Event : Input_Event) return State_Machine.Navigation_Event;

   --  Called when view becomes active
   overriding procedure On_Enter (V : in out Help_View);

   --  Load help content
   procedure Load_Help_Content (V : in out Help_View);

end Hypatia.Views.Help;
