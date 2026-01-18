--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.State_Machine - Implementation
-------------------------------------------------------------------------------

package body Hypatia.State_Machine is

   ---------------------------------------------------------------------------
   --  Check if a transition is valid
   ---------------------------------------------------------------------------

   function Is_Valid_Transition
      (From_State : TUI_State;
       Event      : Navigation_Event) return Boolean
   is
   begin
      if Event = Event_None then
         return False;
      end if;

      case From_State is
         when State_Uninitialized =>
            return Event = Event_Initialize;

         when State_Splash =>
            return Event = Event_Splash_Done;

         when State_Main_Menu =>
            return Event in Event_Go_Scan | Event_Go_Fleet |
                            Event_Go_Registry | Event_Go_Settings |
                            Event_Go_Help | Event_Request_Exit;

         when State_Scan_View | State_Fleet_View |
              State_Registry_View | State_Settings_View | State_Help_View =>
            return Event in Event_Go_Back | Event_Request_Exit | Event_Go_Help;

         when State_Confirm_Exit =>
            return Event in Event_Confirm_Exit | Event_Cancel_Exit;

         when State_Exiting =>
            return Event = Event_Shutdown;

         when State_Terminated =>
            return False;
      end case;
   end Is_Valid_Transition;

   ---------------------------------------------------------------------------
   --  Get the next state for a transition
   ---------------------------------------------------------------------------

   function Next_State
      (Current : TUI_State;
       Event   : Navigation_Event) return TUI_State
   is
   begin
      case Event is
         when Event_None =>
            return Current;

         when Event_Initialize =>
            return State_Splash;

         when Event_Splash_Done =>
            return State_Main_Menu;

         when Event_Go_Scan =>
            return State_Scan_View;

         when Event_Go_Fleet =>
            return State_Fleet_View;

         when Event_Go_Registry =>
            return State_Registry_View;

         when Event_Go_Settings =>
            return State_Settings_View;

         when Event_Go_Help =>
            return State_Help_View;

         when Event_Go_Back =>
            --  Go back to main menu from any view
            case Current is
               when State_Scan_View | State_Fleet_View |
                    State_Registry_View | State_Settings_View | State_Help_View =>
                  return State_Main_Menu;
               when others =>
                  return Current;
            end case;

         when Event_Request_Exit =>
            return State_Confirm_Exit;

         when Event_Confirm_Exit =>
            return State_Exiting;

         when Event_Cancel_Exit =>
            return State_Main_Menu;

         when Event_Shutdown =>
            return State_Terminated;
      end case;
   end Next_State;

   ---------------------------------------------------------------------------
   --  Process an event and transition
   ---------------------------------------------------------------------------

   procedure Process_Event
      (State   : in out TUI_State;
       Event   : Navigation_Event;
       Success : out Boolean)
   is
   begin
      if Event = Event_None then
         Success := False;
         return;
      end if;

      if Is_Valid_Transition (State, Event) then
         State := Next_State (State, Event);
         Success := True;
      else
         Success := False;
      end if;
   end Process_Event;

end Hypatia.State_Machine;
