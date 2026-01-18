--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Widgets - Base widget types and interfaces
--
--  Provides the foundation for all TUI widgets including:
--  - Base widget type with bounds and visibility
--  - Focusable interface for interactive widgets
--  - Common rendering primitives
-------------------------------------------------------------------------------

with Hypatia.Terminal;

package Hypatia.Widgets is

   use Hypatia.Terminal;

   --  Widget bounds (rectangular area)
   type Bounds is record
      X      : Terminal_Width  := 1;
      Y      : Terminal_Height := 1;
      Width  : Terminal_Width  := 10;
      Height : Terminal_Height := 1;
   end record;

   --  Alignment options
   type Horizontal_Align is (Align_Left, Align_Center, Align_Right);
   type Vertical_Align is (Align_Top, Align_Middle, Align_Bottom);

   --  Border style
   type Border_Style is (Border_None, Border_Single, Border_Double, Border_Rounded);

   --  Base widget (abstract)
   type Widget is abstract tagged record
      Area    : Bounds := (1, 1, 10, 1);
      Visible : Boolean := True;
      Focused : Boolean := False;
   end record;

   --  Render the widget to terminal
   procedure Render
      (W    : Widget;
       Term : in out Terminal_State) is abstract;

   --  Handle input event, return True if consumed
   function Handle_Input
      (W     : in out Widget;
       Event : Input_Event) return Boolean is abstract;

   --  Set widget bounds
   procedure Set_Bounds (W : in out Widget'Class; Area : Bounds);

   --  Get widget bounds
   function Get_Bounds (W : Widget'Class) return Bounds;

   --  Set visibility
   procedure Set_Visible (W : in out Widget'Class; Visible : Boolean);

   --  Check visibility
   function Is_Visible (W : Widget'Class) return Boolean;

   --  Set focus
   procedure Set_Focus (W : in out Widget'Class; Focused : Boolean);

   --  Check focus
   function Has_Focus (W : Widget'Class) return Boolean;

   ---------------------------------------------------------------------------
   --  Rendering utilities
   ---------------------------------------------------------------------------

   --  Draw a box with optional title
   procedure Draw_Box
      (Term   : in out Terminal_State;
       Area   : Bounds;
       Style  : Border_Style := Border_Single;
       Title  : String := "");

   --  Draw a horizontal line
   procedure Draw_Horizontal_Line
      (Term   : in out Terminal_State;
       X, Y   : Positive;
       Length : Positive;
       Style  : Border_Style := Border_Single);

   --  Draw a vertical line
   procedure Draw_Vertical_Line
      (Term   : in out Terminal_State;
       X, Y   : Positive;
       Length : Positive;
       Style  : Border_Style := Border_Single);

   --  Fill area with character
   procedure Fill_Area
      (Term : in out Terminal_State;
       Area : Bounds;
       Char : Character := ' ');

   --  Draw text with alignment within bounds
   procedure Draw_Text
      (Term    : in out Terminal_State;
       Area    : Bounds;
       Text    : String;
       H_Align : Horizontal_Align := Align_Left;
       V_Align : Vertical_Align := Align_Top;
       S       : Terminal.Style := Style_Normal);

   --  Truncate text to fit width with ellipsis
   function Truncate (Text : String; Max_Width : Positive) return String;

   --  Pad text to exact width
   function Pad
      (Text  : String;
       Width : Positive;
       Align : Horizontal_Align := Align_Left;
       Fill  : Character := ' ') return String;

end Hypatia.Widgets;
