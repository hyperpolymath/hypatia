--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Widgets.List - Selectable list widget
--
--  A scrollable, selectable list widget for displaying items with:
--  - Keyboard navigation (up/down/page up/page down/home/end)
--  - Selection highlighting
--  - Scrollbar indicator
--  - Custom item rendering
-------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Hypatia.Widgets.List is

   --  Maximum items in list
   Max_Items : constant := 10_000;

   --  List item
   type List_Item is record
      Text    : Unbounded_String := Null_Unbounded_String;
      Data    : Natural := 0;  -- Application-specific data
      Enabled : Boolean := True;
   end record;

   --  Item vector
   package Item_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => List_Item);

   --  List widget
   type List_Widget is new Widget with record
      Items           : Item_Vectors.Vector;
      Selected_Index  : Natural := 0;  -- 0 means no selection
      Scroll_Offset   : Natural := 0;
      Show_Border     : Boolean := True;
      Show_Scrollbar  : Boolean := True;
      Title           : Unbounded_String := Null_Unbounded_String;
      Empty_Message   : Unbounded_String := To_Unbounded_String ("No items");
      Normal_Style    : Terminal.Style := Style_Normal;
      Selected_Style  : Terminal.Style := (Attributes => [Attr_Reverse => True, others => False], others => <>);
      Disabled_Style  : Terminal.Style := Style_Muted;
   end record;

   --  Render the list
   overriding procedure Render
      (W    : List_Widget;
       Term : in out Terminal_State);

   --  Handle input (navigation)
   overriding function Handle_Input
      (W     : in out List_Widget;
       Event : Input_Event) return Boolean;

   --  Add an item to the list
   procedure Add_Item
      (W    : in out List_Widget;
       Text : String;
       Data : Natural := 0;
       Enabled : Boolean := True);

   --  Clear all items
   procedure Clear (W : in out List_Widget);

   --  Get number of items
   function Item_Count (W : List_Widget) return Natural;

   --  Get selected item index (0 if none)
   function Selected (W : List_Widget) return Natural;

   --  Set selected index
   procedure Set_Selected (W : in out List_Widget; Index : Natural);

   --  Get selected item (or empty if none)
   function Get_Selected_Item (W : List_Widget) return List_Item;

   --  Move selection
   procedure Select_Next (W : in out List_Widget);
   procedure Select_Previous (W : in out List_Widget);
   procedure Select_First (W : in out List_Widget);
   procedure Select_Last (W : in out List_Widget);
   procedure Page_Down (W : in out List_Widget);
   procedure Page_Up (W : in out List_Widget);

   --  Ensure selected item is visible
   procedure Ensure_Visible (W : in out List_Widget);

   --  Set title
   procedure Set_Title (W : in out List_Widget; Title : String);

private

   --  Calculate visible area (excluding borders)
   function Visible_Height (W : List_Widget) return Positive;

end Hypatia.Widgets.List;
