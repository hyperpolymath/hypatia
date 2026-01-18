--  SPDX-License-Identifier: PLMP-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia_Main - Main entry point for Hypatia CI/CD TUI
--
--  Neurosymbolic CI/CD Intelligence Platform - Terminal User Interface
-------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;
with Hypatia;
with Hypatia.Application;

procedure Hypatia_Main is
   use Ada.Command_Line;
   use Ada.Text_IO;

   App : Hypatia.Application.App_State;
begin
   --  Check for help flag
   if Argument_Count > 0 then
      declare
         Arg : constant String := Argument (1);
      begin
         if Arg = "--help" or Arg = "-h" then
            Put_Line ("Hypatia v" & Hypatia.Version);
            Put_Line ("Neurosymbolic CI/CD Intelligence Platform - Terminal Interface");
            Put_Line ("");
            Put_Line ("Usage: hypatia [OPTIONS]");
            Put_Line ("");
            Put_Line ("Options:");
            Put_Line ("  -h, --help     Show this help message");
            Put_Line ("  -v, --version  Show version information");
            Put_Line ("");
            Put_Line ("Navigation:");
            Put_Line ("  Arrow keys     Navigate menus and lists");
            Put_Line ("  Enter          Select/confirm");
            Put_Line ("  Escape         Go back/cancel");
            Put_Line ("  q              Quit");
            Put_Line ("  ?              Show help");
            Set_Exit_Status (Exit_Status (Hypatia.Exit_Success));
            return;

         elsif Arg = "--version" or Arg = "-v" then
            Put_Line ("hypatia " & Hypatia.Version);
            Put_Line ("SPARK-verified state machine, Ada 2022 TUI");
            Set_Exit_Status (Exit_Status (Hypatia.Exit_Success));
            return;
         end if;
      end;
   end if;

   --  Initialize and run application
   begin
      Hypatia.Application.Initialize (App);
      Hypatia.Application.Run (App);
      Hypatia.Application.Shutdown (App);
      Set_Exit_Status (Exit_Status (Hypatia.Application.Get_Exit_Code (App)));

   exception
      when others =>
         --  Ensure terminal is restored on error
         Hypatia.Application.Shutdown (App);
         Put_Line (Standard_Error, "Error: TUI terminated unexpectedly");
         Set_Exit_Status (Exit_Status (Hypatia.Exit_General_Error));
   end;

end Hypatia_Main;
