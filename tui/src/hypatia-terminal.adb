--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Terminal - Implementation with POSIX raw mode
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with System;
with Hypatia.POSIX;

package body Hypatia.Terminal is

   package Latin_1 renames Ada.Characters.Latin_1;
   use Hypatia.POSIX;
   use Interfaces.C;

   --  ANSI escape sequences
   ESC : constant Character := Latin_1.ESC;
   CSI : constant String := ESC & "[";

   --  Forward declarations for helper functions
   procedure Raw_Write (Text : String);
   function Query_Terminal_Size return Dimensions;
   function Parse_Escape_Sequence return Input_Event;

   ---------------------------------------------------------------------------
   --  Internal: Write directly to stdout
   ---------------------------------------------------------------------------

   procedure Raw_Write (Text : String) is
   begin
      Ada.Text_IO.Put (Text);
   end Raw_Write;

   ---------------------------------------------------------------------------
   --  termios conversion
   ---------------------------------------------------------------------------

   function To_Raw_Termios is new Ada.Unchecked_Conversion
      (Source => POSIX.Termios_Record, Target => Raw_Termios);

   function From_Raw_Termios is new Ada.Unchecked_Conversion
      (Source => Raw_Termios, Target => POSIX.Termios_Record);

   ---------------------------------------------------------------------------
   --  Get terminal size via ioctl
   ---------------------------------------------------------------------------

   function Query_Terminal_Size return Dimensions is
      ws     : aliased POSIX.Winsize_Record;
      Result : Dimensions := (80, 24);  --  Fallback
   begin
      if POSIX.Ioctl (STDIN_FILENO, unsigned_long (TIOCGWINSZ), ws'Access) = 0
      then
         if ws.ws_col >= unsigned_short (Min_Terminal_Width) and
            ws.ws_col <= unsigned_short (Max_Terminal_Width) and
            ws.ws_row >= unsigned_short (Min_Terminal_Height) and
            ws.ws_row <= unsigned_short (Max_Terminal_Height)
         then
            Result.Width := Terminal_Width (ws.ws_col);
            Result.Height := Terminal_Height (ws.ws_row);
         end if;
      end if;
      return Result;
   exception
      when others =>
         return Result;
   end Query_Terminal_Size;

   ---------------------------------------------------------------------------
   --  Enable raw mode
   ---------------------------------------------------------------------------

   procedure Enable_Raw_Mode (Term : in out Terminal_State) is
      orig : aliased POSIX.Termios_Record;
      raw  : aliased POSIX.Termios_Record;
      rc   : int;
      pragma Unreferenced (rc);
   begin
      if Term.Raw_Mode_Active then
         return;
      end if;

      --  Get current settings
      rc := Tcgetattr (STDIN_FILENO, orig'Access);

      --  Save original for later restoration
      Term.Original_Termios := To_Raw_Termios (orig);

      --  Configure raw mode
      raw := orig;

      --  Input flags: disable break, CR-NL, parity, strip, flow control
      raw.c_iflag := raw.c_iflag and not
         (Tcflag_T (BRKINT) or Tcflag_T (ICRNL) or Tcflag_T (INPCK) or
          Tcflag_T (ISTRIP) or Tcflag_T (IXON));

      --  Output flags: disable post-processing
      raw.c_oflag := raw.c_oflag and not Tcflag_T (OPOST);

      --  Control flags: 8-bit chars
      raw.c_cflag := raw.c_cflag or Tcflag_T (CS8);

      --  Local flags: disable echo, canonical, signals, extended
      raw.c_lflag := raw.c_lflag and not
         (Tcflag_T (ECHO) or Tcflag_T (ICANON) or
          Tcflag_T (IEXTEN) or Tcflag_T (ISIG));

      --  Control chars: return on any input, no timeout
      raw.c_cc (VMIN) := 1;
      raw.c_cc (VTIME) := 0;

      --  Apply settings
      rc := Tcsetattr (STDIN_FILENO, TCSAFLUSH, raw'Access);
      Term.Raw_Mode_Active := True;

   exception
      when others =>
         null;  --  Failed to set raw mode
   end Enable_Raw_Mode;

   ---------------------------------------------------------------------------
   --  Disable raw mode
   ---------------------------------------------------------------------------

   procedure Disable_Raw_Mode (Term : in out Terminal_State) is
      orig : aliased POSIX.Termios_Record;
      rc   : int;
      pragma Unreferenced (rc);
   begin
      if not Term.Raw_Mode_Active then
         return;
      end if;

      --  Restore original settings
      orig := From_Raw_Termios (Term.Original_Termios);
      rc := Tcsetattr (STDIN_FILENO, TCSAFLUSH, orig'Access);
      Term.Raw_Mode_Active := False;

   exception
      when others =>
         Term.Raw_Mode_Active := False;
   end Disable_Raw_Mode;

   ---------------------------------------------------------------------------
   --  Alternate screen buffer
   ---------------------------------------------------------------------------

   procedure Enter_Alternate_Screen (Term : in out Terminal_State) is
   begin
      if not Term.Alt_Screen_Active then
         Raw_Write (CSI & "?1049h");
         Ada.Text_IO.Flush;
         Term.Alt_Screen_Active := True;
      end if;
   end Enter_Alternate_Screen;

   procedure Leave_Alternate_Screen (Term : in out Terminal_State) is
   begin
      if Term.Alt_Screen_Active then
         Raw_Write (CSI & "?1049l");
         Ada.Text_IO.Flush;
         Term.Alt_Screen_Active := False;
      end if;
   end Leave_Alternate_Screen;

   ---------------------------------------------------------------------------
   --  Initialize terminal
   ---------------------------------------------------------------------------

   procedure Initialize (Term : out Terminal_State; Success : out Boolean) is
   begin
      --  Initialize each field individually for limited type
      Term.Initialized := False;
      Term.Raw_Mode_Active := False;
      Term.Alt_Screen_Active := False;
      Term.Dims := Query_Terminal_Size;
      Term.Cursor_Visible := True;
      Term.Current_Style := Style_Normal;
      Term.Buffer := Null_Bounded_String;
      Term.Original_Termios := [others => ASCII.NUL];

      Term.Initialized := True;
      Success := True;

   exception
      when others =>
         Success := False;
   end Initialize;

   --  Overloaded Initialize without Success parameter
   procedure Initialize (Term : in out Terminal_State) is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      --  Initialize each field individually for limited type
      Term.Initialized := False;
      Term.Raw_Mode_Active := False;
      Term.Alt_Screen_Active := False;
      Term.Dims := Query_Terminal_Size;
      Term.Cursor_Visible := True;
      Term.Current_Style := Style_Normal;
      Term.Buffer := Null_Bounded_String;
      Term.Original_Termios := [others => ASCII.NUL];
      Term.Initialized := True;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Finalize terminal
   ---------------------------------------------------------------------------

   procedure Finalize (Term : in out Terminal_State) is
   begin
      if Term.Initialized then
         Flush (Term);
         Reset_Style (Term);
         Show_Cursor (Term);
         Flush (Term);
         Term.Initialized := False;
      end if;
   end Finalize;

   ---------------------------------------------------------------------------
   --  Dimension accessors
   ---------------------------------------------------------------------------

   function Get_Dimensions (Term : Terminal_State) return Dimensions is
   begin
      return Term.Dims;
   end Get_Dimensions;

   function Get_Width (Term : Terminal_State) return Terminal_Width is
   begin
      return Term.Dims.Width;
   end Get_Width;

   function Get_Height (Term : Terminal_State) return Terminal_Height is
   begin
      return Term.Dims.Height;
   end Get_Height;

   procedure Update_Dimensions (Term : in out Terminal_State) is
   begin
      Term.Dims := Query_Terminal_Size;
   end Update_Dimensions;

   ---------------------------------------------------------------------------
   --  Cursor operations
   ---------------------------------------------------------------------------

   procedure Move_Cursor (Term : in out Terminal_State; Pos : Position) is
      Row_Str : constant String := Terminal_Height'Image (Pos.Row);
      Col_Str : constant String := Terminal_Width'Image (Pos.Column);
   begin
      Append (Term.Buffer, CSI);
      Append (Term.Buffer, Row_Str (Row_Str'First + 1 .. Row_Str'Last));
      Append (Term.Buffer, ";");
      Append (Term.Buffer, Col_Str (Col_Str'First + 1 .. Col_Str'Last));
      Append (Term.Buffer, "H");
   end Move_Cursor;

   procedure Hide_Cursor (Term : in out Terminal_State) is
   begin
      Append (Term.Buffer, CSI & "?25l");
      Term.Cursor_Visible := False;
   end Hide_Cursor;

   procedure Show_Cursor (Term : in out Terminal_State) is
   begin
      Append (Term.Buffer, CSI & "?25h");
      Term.Cursor_Visible := True;
   end Show_Cursor;

   ---------------------------------------------------------------------------
   --  Screen operations
   ---------------------------------------------------------------------------

   procedure Clear_Screen (Term : in out Terminal_State) is
   begin
      Append (Term.Buffer, CSI & "2J");
      Move_Cursor (Term, (1, 1));
   end Clear_Screen;

   procedure Clear_Line (Term : in out Terminal_State) is
   begin
      Append (Term.Buffer, CSI & "2K");
   end Clear_Line;

   procedure Clear_To_End_Of_Line (Term : in out Terminal_State) is
   begin
      Append (Term.Buffer, CSI & "K");
   end Clear_To_End_Of_Line;

   ---------------------------------------------------------------------------
   --  Style operations
   ---------------------------------------------------------------------------

   procedure Set_Style (Term : in out Terminal_State; S : Style) is
      Fg_Img : constant String := Natural'Image (Natural (S.Foreground));
      Bg_Img : constant String := Natural'Image (Natural (S.Background));
   begin
      Append (Term.Buffer, CSI);
      Append (Term.Buffer, "0");

      if S.Attributes (Attr_Bold) then
         Append (Term.Buffer, ";1");
      end if;
      if S.Attributes (Attr_Dim) then
         Append (Term.Buffer, ";2");
      end if;
      if S.Attributes (Attr_Italic) then
         Append (Term.Buffer, ";3");
      end if;
      if S.Attributes (Attr_Underline) then
         Append (Term.Buffer, ";4");
      end if;
      if S.Attributes (Attr_Blink) then
         Append (Term.Buffer, ";5");
      end if;
      if S.Attributes (Attr_Reverse) then
         Append (Term.Buffer, ";7");
      end if;
      if S.Attributes (Attr_Hidden) then
         Append (Term.Buffer, ";8");
      end if;
      if S.Attributes (Attr_Strikethrough) then
         Append (Term.Buffer, ";9");
      end if;

      if S.Foreground > 0 then
         Append (Term.Buffer, ";38;5;");
         Append (Term.Buffer, Fg_Img (Fg_Img'First + 1 .. Fg_Img'Last));
      end if;

      if S.Background > 0 then
         Append (Term.Buffer, ";48;5;");
         Append (Term.Buffer, Bg_Img (Bg_Img'First + 1 .. Bg_Img'Last));
      end if;

      Append (Term.Buffer, "m");
      Term.Current_Style := S;
   end Set_Style;

   procedure Reset_Style (Term : in out Terminal_State) is
   begin
      Append (Term.Buffer, CSI & "0m");
      Term.Current_Style := Style_Normal;
   end Reset_Style;

   ---------------------------------------------------------------------------
   --  Output operations
   ---------------------------------------------------------------------------

   procedure Put (Term : in out Terminal_State; Text : String) is
   begin
      Append (Term.Buffer, Text);
   end Put;

   procedure Put (Term : in out Terminal_State; C : Character) is
   begin
      Append (Term.Buffer, C);
   end Put;

   procedure Put_Line (Term : in out Terminal_State; Text : String) is
   begin
      Append (Term.Buffer, Text);
      New_Line (Term);
   end Put_Line;

   procedure New_Line (Term : in out Terminal_State) is
   begin
      Append (Term.Buffer, Latin_1.CR & Latin_1.LF);
   end New_Line;

   procedure Put_Styled
      (Term  : in out Terminal_State;
       Text  : String;
       S     : Style)
   is
      Old_Style : constant Style := Term.Current_Style;
   begin
      Set_Style (Term, S);
      Put (Term, Text);
      Set_Style (Term, Old_Style);
   end Put_Styled;

   ---------------------------------------------------------------------------
   --  Flush output buffer
   ---------------------------------------------------------------------------

   procedure Flush (Term : in out Terminal_State) is
   begin
      if Length (Term.Buffer) > 0 then
         Raw_Write (To_String (Term.Buffer));
         Ada.Text_IO.Flush;
         Term.Buffer := Null_Bounded_String;
      end if;
   end Flush;

   ---------------------------------------------------------------------------
   --  Parse escape sequence into key code
   ---------------------------------------------------------------------------

   function Parse_Escape_Sequence return Input_Event is
      Buf            : String (1 .. 8) := [others => ASCII.NUL];
      Len            : Natural := 0;
      rc             : int;
      Default_Result : constant Input_Event := (Key => Key_Escape, others => <>);
   begin
      --  Read up to 8 more characters with short timeout
      --  In practice, escape sequences come in quickly
      for I in 1 .. 8 loop
         rc := POSIX.C_Read (STDIN_FILENO, Buf (I)'Address, 1);
         if rc <= 0 then
            exit;
         end if;
         Len := I;

         --  Check for complete sequences
         if Len >= 2 and then Buf (1) = '[' then
            case Buf (2) is
               when 'A' => return (Key => Key_Up, others => <>);
               when 'B' => return (Key => Key_Down, others => <>);
               when 'C' => return (Key => Key_Right, others => <>);
               when 'D' => return (Key => Key_Left, others => <>);
               when 'H' => return (Key => Key_Home, others => <>);
               when 'F' => return (Key => Key_End, others => <>);
               when '1' =>
                  if Len >= 3 then
                     case Buf (3) is
                        when '~' => return (Key => Key_Home, others => <>);
                        when others => null;
                     end case;
                  end if;
               when '3' =>
                  if Len >= 3 and then Buf (3) = '~' then
                     return (Key => Key_Delete, others => <>);
                  end if;
               when '4' =>
                  if Len >= 3 and then Buf (3) = '~' then
                     return (Key => Key_End, others => <>);
                  end if;
               when '5' =>
                  if Len >= 3 and then Buf (3) = '~' then
                     return (Key => Key_Page_Up, others => <>);
                  end if;
               when '6' =>
                  if Len >= 3 and then Buf (3) = '~' then
                     return (Key => Key_Page_Down, others => <>);
                  end if;
               when others => null;
            end case;
         elsif Len >= 2 and then Buf (1) = 'O' then
            --  SS3 sequences (F1-F4, some Home/End)
            case Buf (2) is
               when 'P' => return (Key => Key_F1, others => <>);
               when 'Q' => return (Key => Key_F2, others => <>);
               when 'R' => return (Key => Key_F3, others => <>);
               when 'S' => return (Key => Key_F4, others => <>);
               when 'H' => return (Key => Key_Home, others => <>);
               when 'F' => return (Key => Key_End, others => <>);
               when others => null;
            end case;
         end if;
      end loop;

      --  Unknown sequence, return as Escape
      return Default_Result;
   end Parse_Escape_Sequence;

   ---------------------------------------------------------------------------
   --  Read single input event (blocking)
   ---------------------------------------------------------------------------

   function Read_Input (Term : Terminal_State) return Input_Event is
      pragma Unreferenced (Term);
      C      : Character;
      rc     : int;
      Result : Input_Event := (Key => Key_None, others => <>);
   begin
      rc := POSIX.C_Read (STDIN_FILENO, C'Address, 1);
      if rc <= 0 then
         return Result;
      end if;

      case C is
         when Latin_1.ESC =>
            --  Could be escape key or start of escape sequence
            Result := Parse_Escape_Sequence;

         when Latin_1.CR | Latin_1.LF =>
            Result := (Key => Key_Enter, others => <>);

         when Latin_1.HT =>
            Result := (Key => Key_Tab, others => <>);

         when Latin_1.BS =>
            Result := (Key => Key_Backspace, others => <>);

         when Latin_1.DEL =>
            Result := (Key => Key_Backspace, others => <>);

         when ' ' .. '~' =>
            --  Printable ASCII
            Result := (Key => Key_Char, Char => C, others => <>);

         when Latin_1.NUL =>
            --  Ctrl+Space or Ctrl+@
            Result := (Key => Key_Char, Char => ' ', Ctrl => True, others => <>);

         when Character'Val (1) .. Character'Val (7) |
              Character'Val (11) .. Character'Val (12) |
              Character'Val (14) .. Character'Val (26) =>
            --  Ctrl+A through Ctrl+Z (excluding already handled chars)
            Result := (Key  => Key_Char,
                       Char => Character'Val (Character'Pos (C) + 96),
                       Ctrl => True,
                       others => <>);

         when others =>
            Result := (Key => Key_Char, Char => C, others => <>);
      end case;

      return Result;
   end Read_Input;

   ---------------------------------------------------------------------------
   --  Poll for input with timeout
   ---------------------------------------------------------------------------

   function Poll_Input
      (Term    : Terminal_State;
       Timeout : Duration := 0.1) return Input_Event
   is
      readfds : aliased POSIX.FD_Set_Type;
      tv      : aliased POSIX.Timeval_Record;
      rc      : int;
      Secs    : constant Long_Integer := Long_Integer (Timeout);
      USecs   : constant Long_Integer :=
                   Long_Integer ((Timeout - Duration (Secs)) * 1_000_000.0);
   begin
      POSIX.Clear_FD_Set (readfds);
      POSIX.Set_FD (STDIN_FILENO, readfds);

      tv.tv_sec := long (Secs);
      tv.tv_usec := long (USecs);

      rc := POSIX.C_Select (STDIN_FILENO + 1,
                            readfds'Address,
                            System.Null_Address,
                            System.Null_Address,
                            tv'Address);

      if rc > 0 and then POSIX.Is_FD_Set (STDIN_FILENO, readfds) then
         return Read_Input (Term);
      else
         return (Key => Key_None, others => <>);
      end if;
   end Poll_Input;

   ---------------------------------------------------------------------------
   --  Check if input available
   ---------------------------------------------------------------------------

   function Has_Input (Term : Terminal_State) return Boolean is
      readfds : aliased POSIX.FD_Set_Type;
      tv      : aliased POSIX.Timeval_Record := (0, 0);  --  Immediate return
      rc      : int;
      pragma Unreferenced (Term);
   begin
      POSIX.Clear_FD_Set (readfds);
      POSIX.Set_FD (STDIN_FILENO, readfds);

      rc := POSIX.C_Select (STDIN_FILENO + 1,
                            readfds'Address,
                            System.Null_Address,
                            System.Null_Address,
                            tv'Address);

      return rc > 0 and then POSIX.Is_FD_Set (STDIN_FILENO, readfds);
   end Has_Input;

end Hypatia.Terminal;
