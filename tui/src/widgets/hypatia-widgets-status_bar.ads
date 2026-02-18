--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Widgets.Status_Bar - Status bar widget
--
--  A status bar widget typically displayed at the bottom of the screen
--  showing current mode, key hints, and status messages.
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Hypatia.Widgets.Status_Bar is

   --  Key hint entry
   type Key_Hint is record
      Key         : Unbounded_String := Null_Unbounded_String;
      Description : Unbounded_String := Null_Unbounded_String;
   end record;

   --  Maximum key hints
   Max_Hints : constant := 10;

   --  Array of hints
   type Hint_Array is array (1 .. Max_Hints) of Key_Hint;

   --  Status bar widget
   type Status_Bar_Widget is new Widget with record
      Left_Text    : Unbounded_String := Null_Unbounded_String;
      Right_Text   : Unbounded_String := Null_Unbounded_String;
      Hints        : Hint_Array := [others => (others => <>)];
      Hint_Count   : Natural := 0;
      Bar_Style    : Terminal.Style := (Background => Color_Blue, Foreground => Color_White, others => <>);
      Hint_Key_Style : Terminal.Style := (Foreground => Color_Yellow, Attributes => [Attr_Bold => True, others => False], others => <>);
   end record;

   --  Render the status bar
   overriding procedure Render
      (W    : Status_Bar_Widget;
       Term : in out Terminal_State);

   --  Handle input (status bar doesn't handle input)
   overriding function Handle_Input
      (W     : in out Status_Bar_Widget;
       Event : Input_Event) return Boolean;

   --  Set left text (mode indicator, etc.)
   procedure Set_Left_Text (W : in out Status_Bar_Widget; Text : String);

   --  Set right text (status message, etc.)
   procedure Set_Right_Text (W : in out Status_Bar_Widget; Text : String);

   --  Clear all hints
   procedure Clear_Hints (W : in out Status_Bar_Widget);

   --  Add a key hint
   procedure Add_Hint
      (W           : in out Status_Bar_Widget;
       Key         : String;
       Description : String);

   --  Common hint sets
   procedure Set_Navigation_Hints (W : in out Status_Bar_Widget);
   procedure Set_Menu_Hints (W : in out Status_Bar_Widget);
   procedure Set_List_Hints (W : in out Status_Bar_Widget);
   procedure Set_Confirm_Hints (W : in out Status_Bar_Widget);

end Hypatia.Widgets.Status_Bar;
