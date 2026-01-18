--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Views.Fleet - Fleet management view
--
--  Displays and manages multiple repositories in a fleet configuration.
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Hypatia.Widgets.List;
with Hypatia.Widgets.Status_Bar;

package Hypatia.Views.Fleet is

   --  Repository status
   type Repo_Status is (Status_Unknown, Status_Clean, Status_Dirty, Status_Error);

   --  Fleet view
   type Fleet_View is new View with record
      Repo_List    : Widgets.List.List_Widget;
      Status_Bar   : Widgets.Status_Bar.Status_Bar_Widget;
      Fleet_Name   : Unbounded_String := To_Unbounded_String ("Default Fleet");
      Total_Repos  : Natural := 0;
      Clean_Count  : Natural := 0;
      Dirty_Count  : Natural := 0;
      Error_Count  : Natural := 0;
   end record;

   --  Initialize the view
   overriding procedure Initialize
      (V      : in out Fleet_View;
       Width  : Terminal_Width;
       Height : Terminal_Height);

   --  Render the view
   overriding procedure Render
      (V    : Fleet_View;
       Term : in out Terminal_State);

   --  Handle input
   overriding function Handle_Input
      (V     : in out Fleet_View;
       Event : Input_Event) return State_Machine.Navigation_Event;

   --  Called when view becomes active
   overriding procedure On_Enter (V : in out Fleet_View);

   --  Add a repository to the fleet
   procedure Add_Repo
      (V      : in out Fleet_View;
       Name   : String;
       Path   : String;
       Status : Repo_Status := Status_Unknown);

   --  Clear all repositories
   procedure Clear_Repos (V : in out Fleet_View);

   --  Set fleet name
   procedure Set_Fleet_Name (V : in out Fleet_View; Name : String);

   --  Load demo fleet for testing
   procedure Load_Demo_Fleet (V : in out Fleet_View);

end Hypatia.Views.Fleet;
