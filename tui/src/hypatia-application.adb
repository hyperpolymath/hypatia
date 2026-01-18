--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Application - Implementation
-------------------------------------------------------------------------------

package body Hypatia.Application is

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------

   procedure Initialize (App : in out App_State) is
      Width  : constant Terminal_Width  := Get_Width (App.Term);
      Height : constant Terminal_Height := Get_Height (App.Term);
   begin
      --  Initialize terminal
      Initialize (App.Term);
      Enter_Alternate_Screen (App.Term);
      Hide_Cursor (App.Term);
      Enable_Raw_Mode (App.Term);

      --  Initialize state machine
      App.State := State_Splash;

      --  Initialize all views
      Views.Main_Menu.Initialize (App.Menu_View, Width, Height);
      Views.Scan.Initialize (App.Scan_View, Width, Height);
      Views.Fleet.Initialize (App.Fleet_View, Width, Height);
      Views.Registry.Initialize (App.Registry_View, Width, Height);
      Views.Settings.Initialize (App.Settings_View, Width, Height);
      Views.Help.Initialize (App.Help_View, Width, Height);

      --  Load demo data
      Views.Scan.Load_Demo_Results (App.Scan_View);
      Views.Fleet.Load_Demo_Fleet (App.Fleet_View);
      Views.Registry.Load_Demo_Rules (App.Registry_View);

      --  Move to main menu (skip splash for now)
      App.State := State_Main_Menu;
      Views.Main_Menu.On_Enter (App.Menu_View);

      App.Running := True;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Run main loop
   ---------------------------------------------------------------------------

   procedure Run (App : in out App_State) is
      Event     : Input_Event;
      Nav_Event : Navigation_Event;
   begin
      while App.Running loop
         --  Render current view
         Render_Current_View (App);

         --  Wait for input
         Event := Read_Input (App.Term);

         --  Check for resize
         if Event.Key = Key_Resize then
            Handle_Resize (App);
         else
            --  Route input to current view
            case App.State is
               when State_Main_Menu =>
                  Nav_Event := Views.Main_Menu.Handle_Input (App.Menu_View, Event);

               when State_Scan_View =>
                  Nav_Event := Views.Scan.Handle_Input (App.Scan_View, Event);

               when State_Fleet_View =>
                  Nav_Event := Views.Fleet.Handle_Input (App.Fleet_View, Event);

               when State_Registry_View =>
                  Nav_Event := Views.Registry.Handle_Input (App.Registry_View, Event);

               when State_Settings_View =>
                  Nav_Event := Views.Settings.Handle_Input (App.Settings_View, Event);

               when State_Help_View =>
                  Nav_Event := Views.Help.Handle_Input (App.Help_View, Event);

               when State_Confirm_Exit =>
                  --  Handle y/n confirmation
                  if Event.Key = Key_Char then
                     case Event.Char is
                        when 'y' | 'Y' =>
                           Nav_Event := Event_Confirm_Exit;
                        when 'n' | 'N' | Character'Val (27) =>
                           Nav_Event := Event_Cancel_Exit;
                        when others =>
                           Nav_Event := Event_None;
                     end case;
                  elsif Event.Key = Key_Escape then
                     Nav_Event := Event_Cancel_Exit;
                  else
                     Nav_Event := Event_None;
                  end if;

               when others =>
                  Nav_Event := Event_None;
            end case;

            --  Process navigation event
            if Nav_Event /= Event_None then
               Transition (App, Nav_Event);
            end if;
         end if;
      end loop;
   end Run;

   ---------------------------------------------------------------------------
   --  Shutdown
   ---------------------------------------------------------------------------

   procedure Shutdown (App : in out App_State) is
   begin
      Show_Cursor (App.Term);
      Disable_Raw_Mode (App.Term);
      Leave_Alternate_Screen (App.Term);
      Finalize (App.Term);
   end Shutdown;

   ---------------------------------------------------------------------------
   --  Get exit code
   ---------------------------------------------------------------------------

   function Get_Exit_Code (App : App_State) return Exit_Status is
   begin
      return App.Exit_Code;
   end Get_Exit_Code;

   ---------------------------------------------------------------------------
   --  Handle state transition
   ---------------------------------------------------------------------------

   procedure Transition
      (App   : in out App_State;
       Event : Navigation_Event)
   is
      Old_State : constant TUI_State := App.State;
      Success   : Boolean;
   begin
      --  Use state machine to validate and perform transition
      Process_Event (App.State, Event, Success);

      if Success then
         --  Call exit handler for old state
         case Old_State is
            when State_Main_Menu =>
               Views.Main_Menu.On_Exit (App.Menu_View);
            when State_Scan_View =>
               Views.Scan.On_Exit (App.Scan_View);
            when State_Fleet_View =>
               Views.Fleet.On_Exit (App.Fleet_View);
            when State_Registry_View =>
               Views.Registry.On_Exit (App.Registry_View);
            when State_Settings_View =>
               Views.Settings.On_Exit (App.Settings_View);
            when State_Help_View =>
               Views.Help.On_Exit (App.Help_View);
            when others =>
               null;
         end case;

         --  Call enter handler for new state
         case App.State is
            when State_Main_Menu =>
               Views.Main_Menu.On_Enter (App.Menu_View);

            when State_Scan_View =>
               Views.Scan.On_Enter (App.Scan_View);

            when State_Fleet_View =>
               Views.Fleet.On_Enter (App.Fleet_View);

            when State_Registry_View =>
               Views.Registry.On_Enter (App.Registry_View);

            when State_Settings_View =>
               Views.Settings.On_Enter (App.Settings_View);

            when State_Help_View =>
               Views.Help.On_Enter (App.Help_View);

            when State_Terminated =>
               App.Running := False;

            when others =>
               null;
         end case;
      end if;
   end Transition;

   ---------------------------------------------------------------------------
   --  Render current view
   ---------------------------------------------------------------------------

   procedure Render_Current_View (App : in out App_State) is
   begin
      case App.State is
         when State_Main_Menu =>
            Views.Main_Menu.Render (App.Menu_View, App.Term);

         when State_Scan_View =>
            Views.Scan.Render (App.Scan_View, App.Term);

         when State_Fleet_View =>
            Views.Fleet.Render (App.Fleet_View, App.Term);

         when State_Registry_View =>
            Views.Registry.Render (App.Registry_View, App.Term);

         when State_Settings_View =>
            Views.Settings.Render (App.Settings_View, App.Term);

         when State_Help_View =>
            Views.Help.Render (App.Help_View, App.Term);

         when State_Confirm_Exit =>
            --  Draw confirmation dialog over current view
            declare
               W : constant Terminal_Width := Get_Width (App.Term);
               H : constant Terminal_Height := Get_Height (App.Term);
               Dialog_W : constant := 40;
               Dialog_H : constant := 5;
               Dialog_X : constant Terminal_Width := (W - Dialog_W) / 2;
               Dialog_Y : constant Terminal_Height := (H - Dialog_H) / 2;
               Border_H : constant String (1 .. Dialog_W - 2) := (others => '-');
               Padding  : constant String (1 .. Dialog_W - 2) := (others => ' ');
            begin
               --  Draw dialog box (ASCII box drawing)
               Move_Cursor (App.Term, (Dialog_Y, Dialog_X));
               Put_Styled (App.Term, "+" & Border_H & "+", Style_Bold);

               for Row in 1 .. Dialog_H - 2 loop
                  Move_Cursor (App.Term, (Dialog_Y + Row, Dialog_X));
                  Put_Styled (App.Term, "|" & Padding & "|", Style_Bold);
               end loop;

               Move_Cursor (App.Term, (Dialog_Y + Dialog_H - 1, Dialog_X));
               Put_Styled (App.Term, "+" & Border_H & "+", Style_Bold);

               --  Draw message
               Move_Cursor (App.Term, (Dialog_Y + 2, Dialog_X + 9));
               Put_Styled (App.Term, "Exit Hyper TUI? (y/n)", Style_Warning);

               Flush (App.Term);
            end;

         when others =>
            --  Clear screen for unhandled states
            Clear_Screen (App.Term);
            Flush (App.Term);
      end case;
   end Render_Current_View;

   ---------------------------------------------------------------------------
   --  Handle resize
   ---------------------------------------------------------------------------

   procedure Handle_Resize (App : in out App_State) is
      Width  : constant Terminal_Width  := Get_Width (App.Term);
      Height : constant Terminal_Height := Get_Height (App.Term);
   begin
      --  Reinitialize all views with new dimensions
      Views.Main_Menu.Initialize (App.Menu_View, Width, Height);
      Views.Scan.Initialize (App.Scan_View, Width, Height);
      Views.Fleet.Initialize (App.Fleet_View, Width, Height);
      Views.Registry.Initialize (App.Registry_View, Width, Height);
      Views.Settings.Initialize (App.Settings_View, Width, Height);
      Views.Help.Initialize (App.Help_View, Width, Height);
   end Handle_Resize;

end Hypatia.Application;
