--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Views.Scan - Repository scanning view
--
--  Displays scan results, rule violations, and remediation suggestions.
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Hypatia.Widgets.List;
with Hypatia.Widgets.Status_Bar;

package Hypatia.Views.Scan is

   --  Scan result severity
   type Severity_Level is (Sev_Info, Sev_Warning, Sev_Error, Sev_Critical);

   --  Maximum scan results
   Max_Results : constant := 1000;

   --  Scan view
   type Scan_View is new View with record
      Results_List : Widgets.List.List_Widget;
      Status_Bar   : Widgets.Status_Bar.Status_Bar_Widget;
      Title_Text   : Unbounded_String := To_Unbounded_String ("Scan Results");
      Repo_Path    : Unbounded_String := Null_Unbounded_String;
      Total_Issues : Natural := 0;
      Criticals    : Natural := 0;
      Errors       : Natural := 0;
      Warnings     : Natural := 0;
   end record;

   --  Initialize the view
   overriding procedure Initialize
      (V      : in out Scan_View;
       Width  : Terminal_Width;
       Height : Terminal_Height);

   --  Render the view
   overriding procedure Render
      (V    : Scan_View;
       Term : in out Terminal_State);

   --  Handle input
   overriding function Handle_Input
      (V     : in out Scan_View;
       Event : Input_Event) return State_Machine.Navigation_Event;

   --  Called when view becomes active
   overriding procedure On_Enter (V : in out Scan_View);

   --  Add a scan result
   procedure Add_Result
      (V        : in out Scan_View;
       Rule_ID  : String;
       Message  : String;
       Severity : Severity_Level;
       File     : String := "";
       Line     : Natural := 0);

   --  Clear all results
   procedure Clear_Results (V : in out Scan_View);

   --  Set repository path being scanned
   procedure Set_Repo_Path (V : in out Scan_View; Path : String);

   --  Load demo results for testing
   procedure Load_Demo_Results (V : in out Scan_View);

end Hypatia.Views.Scan;
