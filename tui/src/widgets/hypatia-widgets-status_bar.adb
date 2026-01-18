--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Widgets.Status_Bar - Implementation
-------------------------------------------------------------------------------

package body Hypatia.Widgets.Status_Bar is

   ---------------------------------------------------------------------------
   --  Render the status bar
   ---------------------------------------------------------------------------

   overriding procedure Render
      (W    : Status_Bar_Widget;
       Term : in out Terminal_State)
   is
      Bar_Width : constant Positive := W.Area.Width;
      Left_Str  : constant String := To_String (W.Left_Text);
      Right_Str : constant String := To_String (W.Right_Text);
   begin
      if not W.Visible then
         return;
      end if;

      --  Fill the entire bar with background color
      Move_Cursor (Term, (W.Area.Y, W.Area.X));
      Set_Style (Term, W.Bar_Style);
      Put (Term, [1 .. Bar_Width => ' ']);

      --  Draw left text (mode indicator)
      if Left_Str'Length > 0 then
         Move_Cursor (Term, (W.Area.Y, W.Area.X + 1));
         Put_Styled (Term, Left_Str, W.Bar_Style);
      end if;

      --  Draw key hints in the middle
      if W.Hint_Count > 0 then
         declare
            --  Calculate total hints width
            Total_Width : Natural := 0;
            Start_X     : Terminal_Width;
         begin
            for I in 1 .. W.Hint_Count loop
               Total_Width := Total_Width + Length (W.Hints (I).Key) + 1 +
                              Length (W.Hints (I).Description) + 2;
            end loop;

            --  Center the hints
            if Total_Width < Bar_Width - Left_Str'Length - Right_Str'Length - 4 then
               Start_X := W.Area.X + (Bar_Width - Total_Width) / 2;
               Move_Cursor (Term, (W.Area.Y, Start_X));

               for I in 1 .. W.Hint_Count loop
                  --  Key in highlighted style
                  Put_Styled (Term, To_String (W.Hints (I).Key), W.Hint_Key_Style);
                  Put_Styled (Term, ":", W.Bar_Style);
                  Put_Styled (Term, To_String (W.Hints (I).Description), W.Bar_Style);

                  if I < W.Hint_Count then
                     Put_Styled (Term, "  ", W.Bar_Style);
                  end if;
               end loop;
            end if;
         end;
      end if;

      --  Draw right text (status message)
      if Right_Str'Length > 0 and Right_Str'Length < Bar_Width - 2 then
         Move_Cursor (Term, (W.Area.Y, W.Area.X + Bar_Width - Right_Str'Length - 1));
         Put_Styled (Term, Right_Str, W.Bar_Style);
      end if;

      Reset_Style (Term);
   end Render;

   ---------------------------------------------------------------------------
   --  Handle input (status bar doesn't handle input)
   ---------------------------------------------------------------------------

   overriding function Handle_Input
      (W     : in out Status_Bar_Widget;
       Event : Input_Event) return Boolean
   is
      pragma Unreferenced (W, Event);
   begin
      return False;  -- Status bar never consumes input
   end Handle_Input;

   ---------------------------------------------------------------------------
   --  Set left text
   ---------------------------------------------------------------------------

   procedure Set_Left_Text (W : in out Status_Bar_Widget; Text : String) is
   begin
      W.Left_Text := To_Unbounded_String (Text);
   end Set_Left_Text;

   ---------------------------------------------------------------------------
   --  Set right text
   ---------------------------------------------------------------------------

   procedure Set_Right_Text (W : in out Status_Bar_Widget; Text : String) is
   begin
      W.Right_Text := To_Unbounded_String (Text);
   end Set_Right_Text;

   ---------------------------------------------------------------------------
   --  Clear all hints
   ---------------------------------------------------------------------------

   procedure Clear_Hints (W : in out Status_Bar_Widget) is
   begin
      W.Hint_Count := 0;
   end Clear_Hints;

   ---------------------------------------------------------------------------
   --  Add a key hint
   ---------------------------------------------------------------------------

   procedure Add_Hint
      (W           : in out Status_Bar_Widget;
       Key         : String;
       Description : String)
   is
   begin
      if W.Hint_Count < Max_Hints then
         W.Hint_Count := W.Hint_Count + 1;
         W.Hints (W.Hint_Count) := (Key         => To_Unbounded_String (Key),
                                    Description => To_Unbounded_String (Description));
      end if;
   end Add_Hint;

   ---------------------------------------------------------------------------
   --  Common hint sets
   ---------------------------------------------------------------------------

   procedure Set_Navigation_Hints (W : in out Status_Bar_Widget) is
   begin
      Clear_Hints (W);
      Add_Hint (W, "↑↓", "Navigate");
      Add_Hint (W, "Enter", "Select");
      Add_Hint (W, "Esc", "Back");
      Add_Hint (W, "q", "Quit");
   end Set_Navigation_Hints;

   procedure Set_Menu_Hints (W : in out Status_Bar_Widget) is
   begin
      Clear_Hints (W);
      Add_Hint (W, "↑↓", "Navigate");
      Add_Hint (W, "Enter", "Select");
      Add_Hint (W, "q", "Quit");
   end Set_Menu_Hints;

   procedure Set_List_Hints (W : in out Status_Bar_Widget) is
   begin
      Clear_Hints (W);
      Add_Hint (W, "↑↓", "Move");
      Add_Hint (W, "PgUp/Dn", "Page");
      Add_Hint (W, "Enter", "View");
      Add_Hint (W, "Esc", "Back");
   end Set_List_Hints;

   procedure Set_Confirm_Hints (W : in out Status_Bar_Widget) is
   begin
      Clear_Hints (W);
      Add_Hint (W, "y", "Yes");
      Add_Hint (W, "n", "No");
      Add_Hint (W, "Esc", "Cancel");
   end Set_Confirm_Hints;

end Hypatia.Widgets.Status_Bar;
