--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Views.Registry - Registry browser view
--
--  Browse and search the rule registry for CI/CD rules and patterns.
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Hypatia.Widgets.List;
with Hypatia.Widgets.Status_Bar;

package Hypatia.Views.Registry is

   --  Rule category
   type Rule_Category is (Cat_Security, Cat_Quality, Cat_Style, Cat_Performance, Cat_Compliance);

   --  Registry view
   type Registry_View is new View with record
      Rules_List   : Widgets.List.List_Widget;
      Status_Bar   : Widgets.Status_Bar.Status_Bar_Widget;
      Filter_Text  : Unbounded_String := Null_Unbounded_String;
      Total_Rules  : Natural := 0;
      Active_Rules : Natural := 0;
   end record;

   --  Initialize the view
   overriding procedure Initialize
      (V      : in out Registry_View;
       Width  : Terminal_Width;
       Height : Terminal_Height);

   --  Render the view
   overriding procedure Render
      (V    : Registry_View;
       Term : in out Terminal_State);

   --  Handle input
   overriding function Handle_Input
      (V     : in out Registry_View;
       Event : Input_Event) return State_Machine.Navigation_Event;

   --  Called when view becomes active
   overriding procedure On_Enter (V : in out Registry_View);

   --  Add a rule to the list
   procedure Add_Rule
      (V           : in out Registry_View;
       Rule_ID     : String;
       Name        : String;
       Category    : Rule_Category;
       Enabled     : Boolean := True);

   --  Clear all rules
   procedure Clear_Rules (V : in out Registry_View);

   --  Load demo rules
   procedure Load_Demo_Rules (V : in out Registry_View);

end Hypatia.Views.Registry;
