--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Views - Implementation
-------------------------------------------------------------------------------

package body Hypatia.Views is

   ---------------------------------------------------------------------------
   --  Resize view
   ---------------------------------------------------------------------------

   procedure Resize
      (V      : in out View'Class;
       Width  : Terminal_Width;
       Height : Terminal_Height)
   is
   begin
      V.Bounds := (Width, Height);
   end Resize;

end Hypatia.Views;
