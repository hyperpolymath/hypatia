--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia - Root package for cicd-hyper-a Text User Interface
--
--  This package provides the top-level types and constants for the TUI
--  application. Critical state transitions are verified using SPARK.
-------------------------------------------------------------------------------

package Hypatia is
   pragma Preelaborate;

   --  Application version
   Version_Major : constant := 0;
   Version_Minor : constant := 1;
   Version_Patch : constant := 0;
   Version       : constant String := "0.1.0";

   --  Exit status type
   type Exit_Status is range 0 .. 255;

   --  Terminal dimensions constraints
   Min_Terminal_Width  : constant := 80;
   Min_Terminal_Height : constant := 24;
   Max_Terminal_Width  : constant := 500;
   Max_Terminal_Height : constant := 200;

   --  Bounded string types for safety
   subtype Terminal_Width  is Positive range 1 .. Max_Terminal_Width;
   subtype Terminal_Height is Positive range 1 .. Max_Terminal_Height;

   --  Exit codes (matching CLI exit codes)
   Exit_Success          : constant := 0;
   Exit_General_Error    : constant := 1;
   Exit_Invalid_Args     : constant := 2;
   Exit_Config_Error     : constant := 3;
   Exit_IO_Error         : constant := 4;
   Exit_Cancelled        : constant := 5;
   Exit_Internal_Error   : constant := 100;

   --  Color palette indices
   type Color_Index is range 0 .. 255;

   --  Standard colors
   Color_Default    : constant Color_Index := 0;
   Color_Black      : constant Color_Index := 0;
   Color_Red        : constant Color_Index := 1;
   Color_Green      : constant Color_Index := 2;
   Color_Yellow     : constant Color_Index := 3;
   Color_Blue       : constant Color_Index := 4;
   Color_Magenta    : constant Color_Index := 5;
   Color_Cyan       : constant Color_Index := 6;
   Color_White      : constant Color_Index := 7;

   --  Severity colors (matching CLI)
   Color_Critical   : constant Color_Index := Color_Magenta;
   Color_High       : constant Color_Index := Color_Red;
   Color_Medium     : constant Color_Index := Color_Yellow;
   Color_Low        : constant Color_Index := Color_Cyan;
   Color_Info       : constant Color_Index := Color_Blue;

end Hypatia;
