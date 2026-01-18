--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Views - Base view package
--
--  Views represent full-screen layouts that compose widgets and handle
--  application-level input routing.
-------------------------------------------------------------------------------

with Hypatia.Terminal;   use Hypatia.Terminal;
with Hypatia.State_Machine;

package Hypatia.Views is

   --  Base view (abstract)
   type View is abstract tagged record
      Bounds  : Terminal.Dimensions := (80, 24);
      Visible : Boolean := True;
   end record;

   --  Initialize the view with terminal dimensions
   procedure Initialize
      (V      : in out View;
       Width  : Terminal_Width;
       Height : Terminal_Height) is abstract;

   --  Render the view to terminal
   procedure Render
      (V    : View;
       Term : in out Terminal_State) is abstract;

   --  Handle input, return navigation event if state should change
   function Handle_Input
      (V     : in out View;
       Event : Input_Event) return State_Machine.Navigation_Event is abstract;

   --  Called when view becomes active
   procedure On_Enter (V : in out View) is null;

   --  Called when view becomes inactive
   procedure On_Exit (V : in out View) is null;

   --  Update view with terminal dimensions
   procedure Resize
      (V      : in out View'Class;
       Width  : Terminal_Width;
       Height : Terminal_Height);

end Hypatia.Views;
