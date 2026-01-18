--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.State_Machine - State machine for TUI navigation
--
--  This package provides a state machine for navigation between views.
--  Designed for future SPARK verification of valid state transitions.
-------------------------------------------------------------------------------

package Hypatia.State_Machine is

   --  Application states
   type TUI_State is
      (State_Uninitialized,    -- Before initialization
       State_Splash,           -- Splash screen
       State_Main_Menu,        -- Main menu
       State_Scan_View,        -- Repository scan view
       State_Fleet_View,       -- Bot fleet view
       State_Registry_View,    -- Ruleset registry view
       State_Settings_View,    -- Settings view
       State_Help_View,        -- Help view
       State_Confirm_Exit,     -- Exit confirmation
       State_Exiting,          -- Clean shutdown in progress
       State_Terminated);      -- Final state

   --  Navigation events
   type Navigation_Event is
      (Event_None,             -- No event
       Event_Initialize,       -- Start application
       Event_Splash_Done,      -- Splash timeout
       Event_Go_Scan,          -- Go to scan view
       Event_Go_Fleet,         -- Go to fleet view
       Event_Go_Registry,      -- Go to registry view
       Event_Go_Settings,      -- Go to settings view
       Event_Go_Help,          -- Go to help view
       Event_Go_Back,          -- Go back
       Event_Request_Exit,     -- User requests exit
       Event_Confirm_Exit,     -- Confirm exit
       Event_Cancel_Exit,      -- Cancel exit
       Event_Shutdown);        -- Final shutdown

   --  Check if a transition is valid
   function Is_Valid_Transition
      (From_State : TUI_State;
       Event      : Navigation_Event) return Boolean;

   --  Get the next state for a transition
   function Next_State
      (Current : TUI_State;
       Event   : Navigation_Event) return TUI_State;

   --  Process an event and transition to new state
   --  Updates State in place, sets Success to True if transition occurred
   procedure Process_Event
      (State   : in out TUI_State;
       Event   : Navigation_Event;
       Success : out Boolean);

end Hypatia.State_Machine;
