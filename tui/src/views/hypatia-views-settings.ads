--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Views.Settings - Settings view
--
--  Configure TUI and scanner settings.
-------------------------------------------------------------------------------

with Hypatia.Widgets.List;
with Hypatia.Widgets.Status_Bar;

package Hypatia.Views.Settings is

   --  Settings view
   type Settings_View is new View with record
      Settings_List : Widgets.List.List_Widget;
      Status_Bar    : Widgets.Status_Bar.Status_Bar_Widget;
   end record;

   --  Initialize the view
   overriding procedure Initialize
      (V      : in out Settings_View;
       Width  : Terminal_Width;
       Height : Terminal_Height);

   --  Render the view
   overriding procedure Render
      (V    : Settings_View;
       Term : in out Terminal_State);

   --  Handle input
   overriding function Handle_Input
      (V     : in out Settings_View;
       Event : Input_Event) return State_Machine.Navigation_Event;

   --  Called when view becomes active
   overriding procedure On_Enter (V : in out Settings_View);

   --  Refresh settings list
   procedure Refresh_Settings (V : in out Settings_View);

end Hypatia.Views.Settings;
