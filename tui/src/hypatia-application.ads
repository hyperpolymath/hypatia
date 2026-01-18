--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Application - Main TUI application
--
--  Coordinates views, handles state transitions, and manages the main loop.
-------------------------------------------------------------------------------

with Hypatia.Terminal;      use Hypatia.Terminal;
with Hypatia.State_Machine; use Hypatia.State_Machine;
with Hypatia.Views.Main_Menu;
with Hypatia.Views.Scan;
with Hypatia.Views.Fleet;
with Hypatia.Views.Registry;
with Hypatia.Views.Settings;
with Hypatia.Views.Help;

package Hypatia.Application is

   --  Application state
   type App_State is record
      Term         : Terminal_State;
      State        : TUI_State := State_Uninitialized;
      Running      : Boolean := False;
      Exit_Code    : Exit_Status := Exit_Success;
      --  Views
      Menu_View     : Views.Main_Menu.Main_Menu_View;
      Scan_View     : Views.Scan.Scan_View;
      Fleet_View    : Views.Fleet.Fleet_View;
      Registry_View : Views.Registry.Registry_View;
      Settings_View : Views.Settings.Settings_View;
      Help_View     : Views.Help.Help_View;
   end record;

   --  Initialize the application
   procedure Initialize (App : in out App_State);

   --  Run the main event loop
   procedure Run (App : in out App_State);

   --  Shutdown the application
   procedure Shutdown (App : in out App_State);

   --  Get exit code
   function Get_Exit_Code (App : App_State) return Exit_Status;

private

   --  Handle state transition
   procedure Transition
      (App   : in out App_State;
       Event : Navigation_Event);

   --  Render current view
   procedure Render_Current_View (App : in out App_State);

   --  Handle resize
   procedure Handle_Resize (App : in out App_State);

end Hypatia.Application;
