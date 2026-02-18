--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Widgets - Implementation
-------------------------------------------------------------------------------

package body Hypatia.Widgets is

   ---------------------------------------------------------------------------
   --  Forward declarations
   ---------------------------------------------------------------------------

   procedure Get_Box_Chars
      (Style  : Border_Style;
       H, V   : out String;
       TL, TR : out String;
       BL, BR : out String);

   ---------------------------------------------------------------------------
   --  Widget base operations
   ---------------------------------------------------------------------------

   procedure Set_Bounds (W : in out Widget'Class; Area : Bounds) is
   begin
      W.Area := Area;
   end Set_Bounds;

   function Get_Bounds (W : Widget'Class) return Bounds is
   begin
      return W.Area;
   end Get_Bounds;

   procedure Set_Visible (W : in out Widget'Class; Visible : Boolean) is
   begin
      W.Visible := Visible;
   end Set_Visible;

   function Is_Visible (W : Widget'Class) return Boolean is
   begin
      return W.Visible;
   end Is_Visible;

   procedure Set_Focus (W : in out Widget'Class; Focused : Boolean) is
   begin
      W.Focused := Focused;
   end Set_Focus;

   function Has_Focus (W : Widget'Class) return Boolean is
   begin
      return W.Focused;
   end Has_Focus;

   ---------------------------------------------------------------------------
   --  Get box characters for style
   ---------------------------------------------------------------------------

   procedure Get_Box_Chars
      (Style  : Border_Style;
       H, V   : out String;
       TL, TR : out String;
       BL, BR : out String)
   is
   begin
      case Style is
         when Border_None =>
            H  := " ";
            V  := " ";
            TL := " ";
            TR := " ";
            BL := " ";
            BR := " ";
         when Border_Single =>
            H  := Horizontal_Line;
            V  := Vertical_Line;
            TL := Top_Left_Corner;
            TR := Top_Right_Corner;
            BL := Bottom_Left_Corner;
            BR := Bottom_Right_Corner;
         when Border_Double =>
            H  := Double_Horizontal;
            V  := Double_Vertical;
            TL := Double_Top_Left;
            TR := Double_Top_Right;
            BL := Double_Bottom_Left;
            BR := Double_Bottom_Right;
         when Border_Rounded =>
            --  Rounded uses single line but different corners
            H  := Horizontal_Line;
            V  := Vertical_Line;
            TL := "╭";
            TR := "╮";
            BL := "╰";
            BR := "╯";
      end case;
   end Get_Box_Chars;

   ---------------------------------------------------------------------------
   --  Draw a box with optional title
   ---------------------------------------------------------------------------

   procedure Draw_Box
      (Term   : in out Terminal_State;
       Area   : Bounds;
       Style  : Border_Style := Border_Single;
       Title  : String := "")
   is
      H, V, TL, TR, BL, BR : String (1 .. 3);
   begin
      if Style = Border_None then
         return;
      end if;

      Get_Box_Chars (Style, H, V, TL, TR, BL, BR);

      --  Top border
      Move_Cursor (Term, (Area.Y, Area.X));
      Put (Term, TL (1 .. 1));

      if Title'Length > 0 and then Title'Length < Area.Width - 4 then
         --  Title in top border
         declare
            Padding : constant Positive := (Area.Width - 2 - Title'Length) / 2;
         begin
            for I in 1 .. Padding loop
               Put (Term, H (1 .. 1));
            end loop;
            Put (Term, " " & Title & " ");
            for I in 1 .. Area.Width - 2 - Padding - Title'Length - 2 loop
               Put (Term, H (1 .. 1));
            end loop;
         end;
      else
         for I in 1 .. Area.Width - 2 loop
            Put (Term, H (1 .. 1));
         end loop;
      end if;
      Put (Term, TR (1 .. 1));

      --  Side borders
      for Row in Area.Y + 1 .. Area.Y + Area.Height - 2 loop
         Move_Cursor (Term, (Row, Area.X));
         Put (Term, V (1 .. 1));
         Move_Cursor (Term, (Row, Area.X + Area.Width - 1));
         Put (Term, V (1 .. 1));
      end loop;

      --  Bottom border
      Move_Cursor (Term, (Area.Y + Area.Height - 1, Area.X));
      Put (Term, BL (1 .. 1));
      for I in 1 .. Area.Width - 2 loop
         Put (Term, H (1 .. 1));
      end loop;
      Put (Term, BR (1 .. 1));
   end Draw_Box;

   ---------------------------------------------------------------------------
   --  Draw horizontal line
   ---------------------------------------------------------------------------

   procedure Draw_Horizontal_Line
      (Term   : in out Terminal_State;
       X, Y   : Positive;
       Length : Positive;
       Style  : Border_Style := Border_Single)
   is
      H, V, TL, TR, BL, BR : String (1 .. 3);
   begin
      Get_Box_Chars (Style, H, V, TL, TR, BL, BR);
      Move_Cursor (Term, (Y, X));
      for I in 1 .. Length loop
         Put (Term, H (1 .. 1));
      end loop;
   end Draw_Horizontal_Line;

   ---------------------------------------------------------------------------
   --  Draw vertical line
   ---------------------------------------------------------------------------

   procedure Draw_Vertical_Line
      (Term   : in out Terminal_State;
       X, Y   : Positive;
       Length : Positive;
       Style  : Border_Style := Border_Single)
   is
      H, V, TL, TR, BL, BR : String (1 .. 3);
   begin
      Get_Box_Chars (Style, H, V, TL, TR, BL, BR);
      for I in 0 .. Length - 1 loop
         Move_Cursor (Term, (Y + I, X));
         Put (Term, V (1 .. 1));
      end loop;
   end Draw_Vertical_Line;

   ---------------------------------------------------------------------------
   --  Fill area with character
   ---------------------------------------------------------------------------

   procedure Fill_Area
      (Term : in out Terminal_State;
       Area : Bounds;
       Char : Character := ' ')
   is
      Fill_String : constant String (1 .. Area.Width) := (others => Char);
   begin
      for Row in Area.Y .. Area.Y + Area.Height - 1 loop
         Move_Cursor (Term, (Row, Area.X));
         Put (Term, Fill_String);
      end loop;
   end Fill_Area;

   ---------------------------------------------------------------------------
   --  Draw text with alignment
   ---------------------------------------------------------------------------

   procedure Draw_Text
      (Term    : in out Terminal_State;
       Area    : Bounds;
       Text    : String;
       H_Align : Horizontal_Align := Align_Left;
       V_Align : Vertical_Align := Align_Top;
       S       : Terminal.Style := Style_Normal)
   is
      Display_Text : constant String := Truncate (Text, Area.Width);
      X_Offset     : Natural := 0;
      Y_Offset     : Natural := 0;
   begin
      --  Calculate horizontal offset
      case H_Align is
         when Align_Left =>
            X_Offset := 0;
         when Align_Center =>
            X_Offset := (Area.Width - Display_Text'Length) / 2;
         when Align_Right =>
            X_Offset := Area.Width - Display_Text'Length;
      end case;

      --  Calculate vertical offset
      case V_Align is
         when Align_Top =>
            Y_Offset := 0;
         when Align_Middle =>
            Y_Offset := (Area.Height - 1) / 2;
         when Align_Bottom =>
            Y_Offset := Area.Height - 1;
      end case;

      Move_Cursor (Term, (Area.Y + Y_Offset, Area.X + X_Offset));
      Put_Styled (Term, Display_Text, S);
   end Draw_Text;

   ---------------------------------------------------------------------------
   --  Truncate text to fit width
   ---------------------------------------------------------------------------

   function Truncate (Text : String; Max_Width : Positive) return String is
   begin
      if Text'Length <= Max_Width then
         return Text;
      elsif Max_Width <= 3 then
         return Text (Text'First .. Text'First + Max_Width - 1);
      else
         return Text (Text'First .. Text'First + Max_Width - 4) & "...";
      end if;
   end Truncate;

   ---------------------------------------------------------------------------
   --  Pad text to exact width
   ---------------------------------------------------------------------------

   function Pad
      (Text  : String;
       Width : Positive;
       Align : Horizontal_Align := Align_Left;
       Fill  : Character := ' ') return String
   is
      Truncated : constant String := Truncate (Text, Width);
      Padding   : constant Natural := Width - Truncated'Length;
   begin
      if Padding = 0 then
         return Truncated;
      end if;

      case Align is
         when Align_Left =>
            return Truncated & [1 .. Padding => Fill];
         when Align_Right =>
            return [1 .. Padding => Fill] & Truncated;
         when Align_Center =>
            declare
               Left_Pad  : constant Natural := Padding / 2;
               Right_Pad : constant Natural := Padding - Left_Pad;
            begin
               return [1 .. Left_Pad => Fill] & Truncated & [1 .. Right_Pad => Fill];
            end;
      end case;
   end Pad;

end Hypatia.Widgets;
