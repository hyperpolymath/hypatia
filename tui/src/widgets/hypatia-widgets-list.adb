--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Widgets.List - Implementation
-------------------------------------------------------------------------------

with Ada.Containers; use Ada.Containers;

package body Hypatia.Widgets.List is

   ---------------------------------------------------------------------------
   --  Calculate visible height
   ---------------------------------------------------------------------------

   function Visible_Height (W : List_Widget) return Positive is
      Border_Adjust : constant Natural := (if W.Show_Border then 2 else 0);
   begin
      if W.Area.Height > Border_Adjust then
         return W.Area.Height - Border_Adjust;
      else
         return 1;
      end if;
   end Visible_Height;

   ---------------------------------------------------------------------------
   --  Render the list
   ---------------------------------------------------------------------------

   overriding procedure Render
      (W    : List_Widget;
       Term : in out Terminal_State)
   is
      Content_X      : constant Terminal_Width := W.Area.X + (if W.Show_Border then 1 else 0);
      Content_Y      : constant Terminal_Height := W.Area.Y + (if W.Show_Border then 1 else 0);
      Content_Width  : constant Terminal_Width := W.Area.Width - (if W.Show_Border then 2 else 0) - (if W.Show_Scrollbar then 1 else 0);
      Vis_Height     : constant Positive := Visible_Height (W);
      Item_Count_Val : constant Natural := Natural (W.Items.Length);
   begin
      if not W.Visible then
         return;
      end if;

      --  Draw border if enabled
      if W.Show_Border then
         Draw_Box (Term, W.Area, Border_Single, To_String (W.Title));
      end if;

      --  Draw items or empty message
      if Item_Count_Val = 0 then
         --  Show empty message centered
         Draw_Text
            (Term    => Term,
             Area    => (Content_X, Content_Y, Content_Width, Vis_Height),
             Text    => To_String (W.Empty_Message),
             H_Align => Align_Center,
             V_Align => Align_Middle,
             S       => W.Disabled_Style);
      else
         --  Draw visible items
         for I in 0 .. Vis_Height - 1 loop
            declare
               Item_Index : constant Positive := W.Scroll_Offset + I + 1;
               Row_Y      : constant Terminal_Height := Content_Y + Terminal_Height (I);
            begin
               Move_Cursor (Term, (Row_Y, Content_X));

               if Item_Index <= Item_Count_Val then
                  declare
                     Item       : constant List_Item := W.Items (Item_Index);
                     Item_Text  : constant String := Pad (To_String (Item.Text), Content_Width, Align_Left);
                     Item_Style : Terminal.Style;
                  begin
                     --  Determine style
                     if not Item.Enabled then
                        Item_Style := W.Disabled_Style;
                     elsif Item_Index = W.Selected_Index and W.Focused then
                        Item_Style := W.Selected_Style;
                     else
                        Item_Style := W.Normal_Style;
                     end if;

                     Put_Styled (Term, Item_Text, Item_Style);
                  end;
               else
                  --  Empty row
                  Put (Term, [1 .. Content_Width => ' ']);
               end if;
            end;
         end loop;

         --  Draw scrollbar if needed
         if W.Show_Scrollbar and Item_Count_Val > Vis_Height then
            declare
               Scrollbar_X : constant Terminal_Width := W.Area.X + W.Area.Width - (if W.Show_Border then 2 else 1);
               Thumb_Size  : constant Positive := Integer'Max (1, Vis_Height * Vis_Height / Item_Count_Val);
               Thumb_Pos   : constant Natural := W.Scroll_Offset * (Vis_Height - Thumb_Size) / (Item_Count_Val - Vis_Height);
            begin
               for I in 0 .. Vis_Height - 1 loop
                  Move_Cursor (Term, (Content_Y + Terminal_Height (I), Scrollbar_X));
                  if I >= Thumb_Pos and I < Thumb_Pos + Thumb_Size then
                     Put_Styled (Term, "█", Style_Muted);
                  else
                     Put_Styled (Term, "░", Style_Muted);
                  end if;
               end loop;
            end;
         end if;
      end if;
   end Render;

   ---------------------------------------------------------------------------
   --  Handle input
   ---------------------------------------------------------------------------

   overriding function Handle_Input
      (W     : in out List_Widget;
       Event : Input_Event) return Boolean
   is
   begin
      if not W.Focused then
         return False;
      end if;

      case Event.Key is
         when Key_Up =>
            Select_Previous (W);
            return True;

         when Key_Down =>
            Select_Next (W);
            return True;

         when Key_Home =>
            Select_First (W);
            return True;

         when Key_End =>
            Select_Last (W);
            return True;

         when Key_Page_Up =>
            Page_Up (W);
            return True;

         when Key_Page_Down =>
            Page_Down (W);
            return True;

         when Key_Char =>
            --  Could implement type-ahead search here
            return False;

         when others =>
            return False;
      end case;
   end Handle_Input;

   ---------------------------------------------------------------------------
   --  Item management
   ---------------------------------------------------------------------------

   procedure Add_Item
      (W       : in out List_Widget;
       Text    : String;
       Data    : Natural := 0;
       Enabled : Boolean := True)
   is
   begin
      if W.Items.Length < Count_Type (Max_Items) then
         W.Items.Append (List_Item'(Text    => To_Unbounded_String (Text),
                                    Data    => Data,
                                    Enabled => Enabled));

         --  Auto-select first item
         if W.Selected_Index = 0 and Enabled then
            W.Selected_Index := 1;
         end if;
      end if;
   end Add_Item;

   procedure Clear (W : in out List_Widget) is
   begin
      W.Items.Clear;
      W.Selected_Index := 0;
      W.Scroll_Offset := 0;
   end Clear;

   function Item_Count (W : List_Widget) return Natural is
   begin
      return Natural (W.Items.Length);
   end Item_Count;

   function Selected (W : List_Widget) return Natural is
   begin
      return W.Selected_Index;
   end Selected;

   procedure Set_Selected (W : in out List_Widget; Index : Natural) is
   begin
      if Index = 0 or Index <= Natural (W.Items.Length) then
         W.Selected_Index := Index;
         Ensure_Visible (W);
      end if;
   end Set_Selected;

   function Get_Selected_Item (W : List_Widget) return List_Item is
   begin
      if W.Selected_Index > 0 and W.Selected_Index <= Natural (W.Items.Length) then
         return W.Items (W.Selected_Index);
      else
         return (Text    => Null_Unbounded_String,
                 Data    => 0,
                 Enabled => False);
      end if;
   end Get_Selected_Item;

   ---------------------------------------------------------------------------
   --  Navigation
   ---------------------------------------------------------------------------

   procedure Select_Next (W : in out List_Widget) is
   begin
      if W.Selected_Index < Natural (W.Items.Length) then
         W.Selected_Index := W.Selected_Index + 1;
         --  Skip disabled items
         while W.Selected_Index <= Natural (W.Items.Length) and then
               not W.Items (W.Selected_Index).Enabled
         loop
            W.Selected_Index := W.Selected_Index + 1;
         end loop;
         if W.Selected_Index > Natural (W.Items.Length) then
            W.Selected_Index := Natural (W.Items.Length);
         end if;
         Ensure_Visible (W);
      end if;
   end Select_Next;

   procedure Select_Previous (W : in out List_Widget) is
   begin
      if W.Selected_Index > 1 then
         W.Selected_Index := W.Selected_Index - 1;
         --  Skip disabled items
         while W.Selected_Index >= 1 and then
               not W.Items (W.Selected_Index).Enabled
         loop
            if W.Selected_Index > 1 then
               W.Selected_Index := W.Selected_Index - 1;
            else
               exit;
            end if;
         end loop;
         Ensure_Visible (W);
      end if;
   end Select_Previous;

   procedure Select_First (W : in out List_Widget) is
   begin
      if not W.Items.Is_Empty then
         W.Selected_Index := 1;
         --  Skip disabled items
         while W.Selected_Index <= Natural (W.Items.Length) and then
               not W.Items (W.Selected_Index).Enabled
         loop
            W.Selected_Index := W.Selected_Index + 1;
         end loop;
         Ensure_Visible (W);
      end if;
   end Select_First;

   procedure Select_Last (W : in out List_Widget) is
   begin
      if not W.Items.Is_Empty then
         W.Selected_Index := Natural (W.Items.Length);
         --  Skip disabled items
         while W.Selected_Index >= 1 and then
               not W.Items (W.Selected_Index).Enabled
         loop
            W.Selected_Index := W.Selected_Index - 1;
         end loop;
         Ensure_Visible (W);
      end if;
   end Select_Last;

   procedure Page_Down (W : in out List_Widget) is
      Page_Size : constant Positive := Visible_Height (W);
   begin
      for I in 1 .. Page_Size loop
         Select_Next (W);
      end loop;
   end Page_Down;

   procedure Page_Up (W : in out List_Widget) is
      Page_Size : constant Positive := Visible_Height (W);
   begin
      for I in 1 .. Page_Size loop
         Select_Previous (W);
      end loop;
   end Page_Up;

   procedure Ensure_Visible (W : in out List_Widget) is
      Vis_Height : constant Positive := Visible_Height (W);
   begin
      if W.Selected_Index = 0 then
         return;
      end if;

      --  Adjust scroll if selection is above visible area
      if W.Selected_Index <= W.Scroll_Offset then
         W.Scroll_Offset := W.Selected_Index - 1;
      end if;

      --  Adjust scroll if selection is below visible area
      if W.Selected_Index > W.Scroll_Offset + Vis_Height then
         W.Scroll_Offset := W.Selected_Index - Vis_Height;
      end if;
   end Ensure_Visible;

   procedure Set_Title (W : in out List_Widget; Title : String) is
   begin
      W.Title := To_Unbounded_String (Title);
   end Set_Title;

end Hypatia.Widgets.List;
