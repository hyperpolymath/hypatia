--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Terminal - Low-level terminal handling
--
--  Provides ANSI escape sequence based terminal control for:
--  - Cursor positioning and visibility
--  - Color and attribute control
--  - Screen clearing and scrolling
--  - Raw mode input handling
--  - Terminal size detection
-------------------------------------------------------------------------------

with Ada.Strings.Bounded;

package Hypatia.Terminal is

   --  Terminal dimensions
   type Dimensions is record
      Width  : Terminal_Width  := Min_Terminal_Width;
      Height : Terminal_Height := Min_Terminal_Height;
   end record;

   --  Cursor position (1-based)
   type Position is record
      Row    : Terminal_Height := 1;
      Column : Terminal_Width  := 1;
   end record;

   --  Text attributes
   type Attribute is
      (Attr_Normal,
       Attr_Bold,
       Attr_Dim,
       Attr_Italic,
       Attr_Underline,
       Attr_Blink,
       Attr_Reverse,
       Attr_Hidden,
       Attr_Strikethrough);

   --  Attribute set
   type Attribute_Set is array (Attribute) of Boolean
      with Default_Component_Value => False;

   --  Style combining foreground, background and attributes
   type Style is record
      Foreground : Color_Index   := Color_Default;
      Background : Color_Index   := Color_Default;
      Attributes : Attribute_Set := [others => False];
   end record;

   --  Predefined styles
   Style_Normal   : constant Style := (others => <>);
   Style_Bold     : constant Style := (Attributes => [Attr_Bold => True, others => False], others => <>);
   Style_Error    : constant Style := (Foreground => Color_Red, Attributes => [Attr_Bold => True, others => False], others => <>);
   Style_Warning  : constant Style := (Foreground => Color_Yellow, others => <>);
   Style_Success  : constant Style := (Foreground => Color_Green, others => <>);
   Style_Info     : constant Style := (Foreground => Color_Blue, others => <>);
   Style_Muted    : constant Style := (Attributes => [Attr_Dim => True, others => False], others => <>);

   --  Key codes for input handling
   type Key_Code is
      (Key_None,
       Key_Enter,
       Key_Tab,
       Key_Backspace,
       Key_Escape,
       Key_Up,
       Key_Down,
       Key_Left,
       Key_Right,
       Key_Home,
       Key_End,
       Key_Page_Up,
       Key_Page_Down,
       Key_Insert,
       Key_Delete,
       Key_F1, Key_F2, Key_F3, Key_F4, Key_F5, Key_F6,
       Key_F7, Key_F8, Key_F9, Key_F10, Key_F11, Key_F12,
       Key_Char,    -- Regular character
       Key_Resize); -- Terminal resize event

   --  Input event
   type Input_Event is record
      Key  : Key_Code := Key_None;
      Char : Character := ASCII.NUL;  -- Valid when Key = Key_Char
      Alt  : Boolean := False;        -- Alt modifier
      Ctrl : Boolean := False;        -- Ctrl modifier
   end record;

   --  Bounded string for output buffering
   package Output_Buffer is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 4096);
   use Output_Buffer;

   --  Terminal state
   type Terminal_State is limited private;

   --  Initialize terminal (enter raw mode, hide cursor, etc.)
   procedure Initialize (Term : out Terminal_State; Success : out Boolean);

   --  Initialize terminal (simplified, no success flag)
   procedure Initialize (Term : in out Terminal_State);

   --  Finalize terminal (restore original state)
   procedure Finalize (Term : in out Terminal_State);

   --  Get terminal dimensions
   function Get_Dimensions (Term : Terminal_State) return Dimensions;

   --  Update dimensions (call on SIGWINCH)
   procedure Update_Dimensions (Term : in out Terminal_State);

   --  Cursor operations
   procedure Move_Cursor (Term : in out Terminal_State; Pos : Position);
   procedure Hide_Cursor (Term : in out Terminal_State);
   procedure Show_Cursor (Term : in out Terminal_State);

   --  Screen operations
   procedure Clear_Screen (Term : in out Terminal_State);
   procedure Clear_Line (Term : in out Terminal_State);
   procedure Clear_To_End_Of_Line (Term : in out Terminal_State);

   --  Style operations
   procedure Set_Style (Term : in out Terminal_State; S : Style);
   procedure Reset_Style (Term : in out Terminal_State);

   --  Output operations
   procedure Put (Term : in out Terminal_State; Text : String);
   procedure Put (Term : in out Terminal_State; C : Character);
   procedure Put_Line (Term : in out Terminal_State; Text : String);
   procedure New_Line (Term : in out Terminal_State);

   --  Styled output
   procedure Put_Styled
      (Term  : in out Terminal_State;
       Text  : String;
       S     : Style);

   --  Flush output buffer to terminal
   procedure Flush (Term : in out Terminal_State);

   --  Input operations
   function Poll_Input
      (Term    : Terminal_State;
       Timeout : Duration := 0.1) return Input_Event;

   --  Blocking input read
   function Read_Input (Term : Terminal_State) return Input_Event;

   --  Check if input is available
   function Has_Input (Term : Terminal_State) return Boolean;

   --  Raw mode control
   procedure Enable_Raw_Mode (Term : in out Terminal_State);
   procedure Disable_Raw_Mode (Term : in out Terminal_State);

   --  Alternate screen buffer
   procedure Enter_Alternate_Screen (Term : in out Terminal_State);
   procedure Leave_Alternate_Screen (Term : in out Terminal_State);

   --  Convenience dimension accessors
   function Get_Width (Term : Terminal_State) return Terminal_Width;
   function Get_Height (Term : Terminal_State) return Terminal_Height;

   --  Box drawing characters (Unicode)
   Horizontal_Line    : constant String := "─";
   Vertical_Line      : constant String := "│";
   Top_Left_Corner    : constant String := "┌";
   Top_Right_Corner   : constant String := "┐";
   Bottom_Left_Corner : constant String := "└";
   Bottom_Right_Corner : constant String := "┘";
   T_Down             : constant String := "┬";
   T_Up               : constant String := "┴";
   T_Right            : constant String := "├";
   T_Left             : constant String := "┤";
   Cross              : constant String := "┼";

   --  Double-line box drawing
   Double_Horizontal   : constant String := "═";
   Double_Vertical     : constant String := "║";
   Double_Top_Left     : constant String := "╔";
   Double_Top_Right    : constant String := "╗";
   Double_Bottom_Left  : constant String := "╚";
   Double_Bottom_Right : constant String := "╝";

private

   --  Forward declare POSIX types needed
   type Raw_Termios is array (1 .. 60) of Character;  -- Opaque storage for termios

   type Terminal_State is limited record
      Initialized      : Boolean := False;
      Raw_Mode_Active  : Boolean := False;
      Alt_Screen_Active : Boolean := False;
      Dims             : Dimensions := (Min_Terminal_Width, Min_Terminal_Height);
      Cursor_Visible   : Boolean := True;
      Current_Style    : Style := Style_Normal;
      Buffer           : Bounded_String := Null_Bounded_String;
      Original_Termios : Raw_Termios := [others => ASCII.NUL];
   end record;

end Hypatia.Terminal;
