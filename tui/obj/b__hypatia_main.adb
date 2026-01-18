pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__hypatia_main.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__hypatia_main.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E075 : Short_Integer; pragma Import (Ada, E075, "system__os_lib_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "ada__exceptions_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
   E024 : Short_Integer; pragma Import (Ada, E024, "system__exception_table_E");
   E040 : Short_Integer; pragma Import (Ada, E040, "ada__containers_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "ada__io_exceptions_E");
   E031 : Short_Integer; pragma Import (Ada, E031, "ada__numerics_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "ada__strings_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "ada__strings__maps_E");
   E060 : Short_Integer; pragma Import (Ada, E060, "ada__strings__maps__constants_E");
   E045 : Short_Integer; pragma Import (Ada, E045, "interfaces__c_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exceptions_E");
   E086 : Short_Integer; pragma Import (Ada, E086, "system__object_reader_E");
   E050 : Short_Integer; pragma Import (Ada, E050, "system__dwarf_lines_E");
   E017 : Short_Integer; pragma Import (Ada, E017, "system__soft_links__initialize_E");
   E039 : Short_Integer; pragma Import (Ada, E039, "system__traceback__symbolic_E");
   E154 : Short_Integer; pragma Import (Ada, E154, "ada__assertions_E");
   E107 : Short_Integer; pragma Import (Ada, E107, "ada__strings__utf_encoding_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "ada__tags_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "ada__strings__text_buffers_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "ada__streams_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "system__file_control_block_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "system__finalization_root_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "ada__finalization_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "system__file_io_E");
   E192 : Short_Integer; pragma Import (Ada, E192, "system__storage_pools_E");
   E194 : Short_Integer; pragma Import (Ada, E194, "system__storage_pools__subpools_E");
   E179 : Short_Integer; pragma Import (Ada, E179, "ada__strings__unbounded_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "ada__text_io_E");
   E188 : Short_Integer; pragma Import (Ada, E188, "system__pool_global_E");
   E148 : Short_Integer; pragma Import (Ada, E148, "hypatia__posix_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "hypatia__state_machine_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "hypatia__terminal_E");
   E162 : Short_Integer; pragma Import (Ada, E162, "hypatia__views_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "hypatia__widgets_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "hypatia__widgets__list_E");
   E200 : Short_Integer; pragma Import (Ada, E200, "hypatia__widgets__status_bar_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "hypatia__views__fleet_E");
   E202 : Short_Integer; pragma Import (Ada, E202, "hypatia__views__help_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "hypatia__views__main_menu_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "hypatia__views__registry_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "hypatia__views__scan_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "hypatia__views__settings_E");
   E142 : Short_Integer; pragma Import (Ada, E142, "hypatia__application_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E210 := E210 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "hypatia__views__settings__finalize_spec");
      begin
         F1;
      end;
      E208 := E208 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "hypatia__views__scan__finalize_spec");
      begin
         F2;
      end;
      E206 := E206 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "hypatia__views__registry__finalize_spec");
      begin
         F3;
      end;
      E204 := E204 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "hypatia__views__main_menu__finalize_spec");
      begin
         F4;
      end;
      E202 := E202 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "hypatia__views__help__finalize_spec");
      begin
         F5;
      end;
      E171 := E171 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "hypatia__views__fleet__finalize_spec");
      begin
         F6;
      end;
      E200 := E200 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "hypatia__widgets__status_bar__finalize_spec");
      begin
         F7;
      end;
      E184 := E184 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "hypatia__widgets__list__finalize_spec");
      begin
         F8;
      end;
      E188 := E188 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__pool_global__finalize_spec");
      begin
         F9;
      end;
      E121 := E121 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "ada__text_io__finalize_spec");
      begin
         F10;
      end;
      E179 := E179 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "ada__strings__unbounded__finalize_spec");
      begin
         F11;
      end;
      E194 := E194 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__storage_pools__subpools__finalize_spec");
      begin
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "system__file_io__finalize_body");
      begin
         E131 := E131 - 1;
         F13;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Exception_Tracebacks : Integer;
      pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
      Exception_Tracebacks_Symbolic : Integer;
      pragma Import (C, Exception_Tracebacks_Symbolic, "__gl_exception_tracebacks_symbolic");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");
      Interrupts_Default_To_System : Integer;
      pragma Import (C, Interrupts_Default_To_System, "__gl_interrupts_default_to_system");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Exception_Tracebacks := 1;
      Exception_Tracebacks_Symbolic := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E024 := E024 + 1;
      Ada.Containers'Elab_Spec;
      E040 := E040 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E070 := E070 + 1;
      Ada.Numerics'Elab_Spec;
      E031 := E031 + 1;
      Ada.Strings'Elab_Spec;
      E055 := E055 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E057 := E057 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E060 := E060 + 1;
      Interfaces.C'Elab_Spec;
      E045 := E045 + 1;
      System.Exceptions'Elab_Spec;
      E025 := E025 + 1;
      System.Object_Reader'Elab_Spec;
      E086 := E086 + 1;
      System.Dwarf_Lines'Elab_Spec;
      E050 := E050 + 1;
      System.Os_Lib'Elab_Body;
      E075 := E075 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E017 := E017 + 1;
      E015 := E015 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E039 := E039 + 1;
      E011 := E011 + 1;
      Ada.Assertions'Elab_Spec;
      E154 := E154 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E107 := E107 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E115 := E115 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E105 := E105 + 1;
      Ada.Streams'Elab_Spec;
      E123 := E123 + 1;
      System.File_Control_Block'Elab_Spec;
      E139 := E139 + 1;
      System.Finalization_Root'Elab_Spec;
      E134 := E134 + 1;
      Ada.Finalization'Elab_Spec;
      E132 := E132 + 1;
      System.File_Io'Elab_Body;
      E131 := E131 + 1;
      System.Storage_Pools'Elab_Spec;
      E192 := E192 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E194 := E194 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E179 := E179 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E121 := E121 + 1;
      System.Pool_Global'Elab_Spec;
      E188 := E188 + 1;
      E148 := E148 + 1;
      E144 := E144 + 1;
      Hypatia.Terminal'Elab_Spec;
      E146 := E146 + 1;
      Hypatia.Views'Elab_Spec;
      E162 := E162 + 1;
      Hypatia.Widgets'Elab_Spec;
      E173 := E173 + 1;
      Hypatia.Widgets.List'Elab_Spec;
      Hypatia.Widgets.List'Elab_Body;
      E184 := E184 + 1;
      Hypatia.Widgets.Status_Bar'Elab_Spec;
      Hypatia.Widgets.Status_Bar'Elab_Body;
      E200 := E200 + 1;
      Hypatia.Views.Fleet'Elab_Spec;
      Hypatia.Views.Fleet'Elab_Body;
      E171 := E171 + 1;
      Hypatia.Views.Help'Elab_Spec;
      Hypatia.Views.Help'Elab_Body;
      E202 := E202 + 1;
      Hypatia.Views.Main_Menu'Elab_Spec;
      Hypatia.Views.Main_Menu'Elab_Body;
      E204 := E204 + 1;
      Hypatia.Views.Registry'Elab_Spec;
      Hypatia.Views.Registry'Elab_Body;
      E206 := E206 + 1;
      Hypatia.Views.Scan'Elab_Spec;
      Hypatia.Views.Scan'Elab_Body;
      E208 := E208 + 1;
      Hypatia.Views.Settings'Elab_Spec;
      Hypatia.Views.Settings'Elab_Body;
      E210 := E210 + 1;
      E142 := E142 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_hypatia_main");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia-posix.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia-state_machine.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia-terminal.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia-views.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia-widgets.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia-widgets-list.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia-widgets-status_bar.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia-views-fleet.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia-views-help.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia-views-main_menu.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia-views-registry.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia-views-scan.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia-views-settings.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia-application.o
   --   /var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/hypatia_main.o
   --   -L/var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/
   --   -L/var/mnt/eclipse/repos/cicd-hyper-a/tui/obj/
   --   -L/usr/lib/gcc/x86_64-redhat-linux/15/adalib/
   --   -static
   --   -lgnat
   --   -ldl
--  END Object file/option list   

end ada_main;
