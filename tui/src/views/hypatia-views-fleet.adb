--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.Views.Fleet - Implementation
-------------------------------------------------------------------------------

with Hypatia.Widgets; use Hypatia.Widgets;

package body Hypatia.Views.Fleet is

   use Hypatia.Widgets.List;
   use Hypatia.Widgets.Status_Bar;

   ---------------------------------------------------------------------------
   --  Forward declarations
   ---------------------------------------------------------------------------

   function Status_Indicator (S : Repo_Status) return String;

   ---------------------------------------------------------------------------
   --  Status indicator
   ---------------------------------------------------------------------------

   function Status_Indicator (S : Repo_Status) return String is
   begin
      case S is
         when Status_Unknown => return "[?] ";
         when Status_Clean   => return "[✓] ";
         when Status_Dirty   => return "[*] ";
         when Status_Error   => return "[!] ";
      end case;
   end Status_Indicator;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------

   overriding procedure Initialize
      (V      : in out Fleet_View;
       Width  : Terminal_Width;
       Height : Terminal_Height)
   is
      List_Height : constant Terminal_Height := Height - 6;
   begin
      V.Bounds := (Width, Height);

      --  Configure repository list
      V.Repo_List.Area := (2, 4, Width - 4, List_Height);
      V.Repo_List.Show_Border := True;
      V.Repo_List.Show_Scrollbar := True;
      V.Repo_List.Focused := True;
      V.Repo_List.Empty_Message := To_Unbounded_String ("No repositories in fleet. Press 'a' to add.");
      Set_Title (V.Repo_List, "Fleet Repositories");

      --  Configure status bar
      V.Status_Bar.Area := (1, Height, Width, 1);
      Set_Left_Text (V.Status_Bar, " FLEET ");
      Set_List_Hints (V.Status_Bar);
   end Initialize;

   ---------------------------------------------------------------------------
   --  Render
   ---------------------------------------------------------------------------

   overriding procedure Render
      (V    : Fleet_View;
       Term : in out Terminal_State)
   is
   begin
      Clear_Screen (Term);

      --  Draw title
      Move_Cursor (Term, (1, 2));
      Put_Styled (Term, "Fleet: ", Style_Bold);
      Put_Styled (Term, To_String (V.Fleet_Name), Style_Normal);

      --  Draw summary
      Move_Cursor (Term, (2, 2));
      Put_Styled (Term, "Repos: ", Style_Normal);
      Put_Styled (Term, Natural'Image (V.Total_Repos), Style_Bold);

      Put_Styled (Term, "  Clean: ", Style_Normal);
      Put_Styled (Term, Natural'Image (V.Clean_Count), Style_Success);

      Put_Styled (Term, "  Modified: ", Style_Normal);
      if V.Dirty_Count > 0 then
         Put_Styled (Term, Natural'Image (V.Dirty_Count), Style_Warning);
      else
         Put_Styled (Term, " 0", Style_Normal);
      end if;

      Put_Styled (Term, "  Errors: ", Style_Normal);
      if V.Error_Count > 0 then
         Put_Styled (Term, Natural'Image (V.Error_Count), Style_Error);
      else
         Put_Styled (Term, " 0", Style_Normal);
      end if;

      --  Draw repository list
      Render (V.Repo_List, Term);

      --  Draw status bar
      Render (V.Status_Bar, Term);

      Flush (Term);
   end Render;

   ---------------------------------------------------------------------------
   --  Handle input
   ---------------------------------------------------------------------------

   overriding function Handle_Input
      (V     : in out Fleet_View;
       Event : Input_Event) return State_Machine.Navigation_Event
   is
      use State_Machine;
   begin
      case Event.Key is
         when Key_Escape =>
            return Event_Go_Back;

         when Key_Char =>
            case Event.Char is
               when 'q' | 'Q' =>
                  return Event_Go_Back;
               when 'a' | 'A' =>
                  --  Add repository (demo loads demo fleet)
                  Load_Demo_Fleet (V);
                  return Event_None;
               when 'r' | 'R' =>
                  --  Refresh status
                  return Event_None;
               when 's' | 'S' =>
                  --  Scan selected repo
                  return Event_Go_Scan;
               when '?' =>
                  return Event_Go_Help;
               when others =>
                  null;
            end case;

         when Key_Enter =>
            --  Open selected repo in scan view
            return Event_Go_Scan;

         when others =>
            null;
      end case;

      --  Let list handle navigation
      if Handle_Input (V.Repo_List, Event) then
         null;
      end if;

      return Event_None;
   end Handle_Input;

   ---------------------------------------------------------------------------
   --  On Enter
   ---------------------------------------------------------------------------

   overriding procedure On_Enter (V : in out Fleet_View) is
   begin
      Set_Focus (V.Repo_List, True);
      Clear_Hints (V.Status_Bar);
      Add_Hint (V.Status_Bar, "↑↓", "Navigate");
      Add_Hint (V.Status_Bar, "Enter", "Scan");
      Add_Hint (V.Status_Bar, "a", "Add");
      Add_Hint (V.Status_Bar, "Esc", "Back");
   end On_Enter;

   ---------------------------------------------------------------------------
   --  Add a repository
   ---------------------------------------------------------------------------

   procedure Add_Repo
      (V      : in out Fleet_View;
       Name   : String;
       Path   : String;
       Status : Repo_Status := Status_Unknown)
   is
      Display : Unbounded_String;
   begin
      Append (Display, Status_Indicator (Status));
      Append (Display, Name);
      Append (Display, "  ");
      Append (Display, Path);

      Add_Item (V.Repo_List, To_String (Display), Repo_Status'Pos (Status));

      V.Total_Repos := V.Total_Repos + 1;
      case Status is
         when Status_Clean => V.Clean_Count := V.Clean_Count + 1;
         when Status_Dirty => V.Dirty_Count := V.Dirty_Count + 1;
         when Status_Error => V.Error_Count := V.Error_Count + 1;
         when Status_Unknown => null;
      end case;
   end Add_Repo;

   ---------------------------------------------------------------------------
   --  Clear all repositories
   ---------------------------------------------------------------------------

   procedure Clear_Repos (V : in out Fleet_View) is
   begin
      Clear (V.Repo_List);
      V.Total_Repos := 0;
      V.Clean_Count := 0;
      V.Dirty_Count := 0;
      V.Error_Count := 0;
   end Clear_Repos;

   ---------------------------------------------------------------------------
   --  Set fleet name
   ---------------------------------------------------------------------------

   procedure Set_Fleet_Name (V : in out Fleet_View; Name : String) is
   begin
      V.Fleet_Name := To_Unbounded_String (Name);
   end Set_Fleet_Name;

   ---------------------------------------------------------------------------
   --  Load demo fleet
   ---------------------------------------------------------------------------

   procedure Load_Demo_Fleet (V : in out Fleet_View) is
   begin
      Clear_Repos (V);
      Set_Fleet_Name (V, "Hyperpolymath Fleet");

      Add_Repo (V, "cicd-hyper-a", "/var/mnt/eclipse/repos/cicd-hyper-a", Status_Dirty);
      Add_Repo (V, "bunsenite", "/var/mnt/eclipse/repos/bunsenite", Status_Clean);
      Add_Repo (V, "gitbot-fleet", "/var/mnt/eclipse/repos/gitbot-fleet", Status_Clean);
      Add_Repo (V, "robot-repo-automaton", "/var/mnt/eclipse/repos/robot-repo-automaton", Status_Dirty);
      Add_Repo (V, "finishing-bot", "/var/mnt/eclipse/repos/finishing-bot", Status_Clean);
      Add_Repo (V, "glambot", "/var/mnt/eclipse/repos/bots/glambot", Status_Error);
      Add_Repo (V, "palimpsest-license", "/var/mnt/eclipse/repos/palimpsest-license", Status_Clean);
      Add_Repo (V, "ubicity", "/var/mnt/eclipse/repos/ubicity", Status_Clean);
   end Load_Demo_Fleet;

end Hypatia.Views.Fleet;
