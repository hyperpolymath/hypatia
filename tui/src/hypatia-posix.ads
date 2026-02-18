--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.POSIX - POSIX/termios bindings for terminal control
--
--  Low-level C bindings for terminal raw mode and size detection.
-------------------------------------------------------------------------------

with Interfaces.C;
with System;

package Hypatia.POSIX is

   use Interfaces.C;

   --  File descriptors
   STDIN_FILENO  : constant := 0;
   STDOUT_FILENO : constant := 1;

   --  termios constants (Linux values)
   NCCS : constant := 32;

   --  c_iflag bits
   IGNBRK  : constant := 16#0001#;
   BRKINT  : constant := 16#0002#;
   IGNPAR  : constant := 16#0004#;
   PARMRK  : constant := 16#0008#;
   INPCK   : constant := 16#0010#;
   ISTRIP  : constant := 16#0020#;
   INLCR   : constant := 16#0040#;
   IGNCR   : constant := 16#0080#;
   ICRNL   : constant := 16#0100#;
   IUCLC   : constant := 16#0200#;
   IXON    : constant := 16#0400#;
   IXANY   : constant := 16#0800#;
   IXOFF   : constant := 16#1000#;

   --  c_oflag bits
   OPOST   : constant := 16#0001#;

   --  c_lflag bits
   ISIG    : constant := 16#0001#;
   ICANON  : constant := 16#0002#;
   ECHO    : constant := 16#0008#;
   ECHOE   : constant := 16#0010#;
   ECHOK   : constant := 16#0020#;
   ECHONL  : constant := 16#0040#;
   IEXTEN  : constant := 16#8000#;

   --  c_cflag bits
   CSIZE   : constant := 16#0030#;
   CS8     : constant := 16#0030#;
   PARENB  : constant := 16#0100#;

   --  tcsetattr actions
   TCSANOW   : constant := 0;
   TCSADRAIN : constant := 1;
   TCSAFLUSH : constant := 2;

   --  ioctl request for window size (Linux)
   TIOCGWINSZ : constant := 16#5413#;

   --  c_cc indices
   VMIN  : constant := 6;
   VTIME : constant := 5;

   --  termios structure
   type Cc_T is new unsigned_char;
   type Cc_Array is array (0 .. NCCS - 1) of Cc_T;
   type Tcflag_T is new unsigned;
   type Speed_T is new unsigned;

   type Termios_Record is record
      c_iflag  : Tcflag_T;
      c_oflag  : Tcflag_T;
      c_cflag  : Tcflag_T;
      c_lflag  : Tcflag_T;
      c_line   : Cc_T;
      c_cc     : Cc_Array;
      c_ispeed : Speed_T;
      c_ospeed : Speed_T;
   end record;
   pragma Convention (C, Termios_Record);

   --  winsize structure for TIOCGWINSZ
   type Winsize_Record is record
      ws_row    : unsigned_short;
      ws_col    : unsigned_short;
      ws_xpixel : unsigned_short;
      ws_ypixel : unsigned_short;
   end record;
   pragma Convention (C, Winsize_Record);

   --  termios functions
   function Tcgetattr (fd : int; termios_p : access Termios_Record) return int;
   pragma Import (C, Tcgetattr, "tcgetattr");

   function Tcsetattr
     (fd        : int;
      actions   : int;
      termios_p : access Termios_Record) return int;
   pragma Import (C, Tcsetattr, "tcsetattr");

   --  ioctl for window size
   function Ioctl
     (fd      : int;
      request : unsigned_long;
      arg     : access Winsize_Record) return int;
   pragma Import (C, Ioctl, "ioctl");

   --  read/write for low-level I/O
   function C_Read
     (fd    : int;
      buf   : System.Address;
      count : size_t) return int;
   pragma Import (C, C_Read, "read");

   --  fcntl for non-blocking I/O
   F_GETFL    : constant := 3;
   F_SETFL    : constant := 4;
   O_NONBLOCK : constant := 2048;

   function Fcntl_Getfl (fd : int; cmd : int) return int;
   pragma Import (C, Fcntl_Getfl, "fcntl");

   function Fcntl_Setfl (fd : int; cmd : int; arg : int) return int;
   pragma Import (C, Fcntl_Setfl, "fcntl");

   --  select/poll for input detection
   function C_Select
     (nfds      : int;
      readfds   : System.Address;
      writefds  : System.Address;
      exceptfds : System.Address;
      timeout   : System.Address) return int;
   pragma Import (C, C_Select, "select");

   --  timeval for select
   type Timeval_Record is record
      tv_sec  : long;
      tv_usec : long;
   end record;
   pragma Convention (C, Timeval_Record);

   --  fd_set (simplified - assumes max 64 fds)
   type FD_Set_Type is array (0 .. 15) of unsigned_long;
   pragma Convention (C, FD_Set_Type);

   --  fd_set macros as procedures (renamed to avoid conflict)
   procedure Clear_FD_Set (fds : out FD_Set_Type);
   procedure Set_FD (fd : int; fds : in out FD_Set_Type);
   function Is_FD_Set (fd : int; fds : FD_Set_Type) return Boolean;

end Hypatia.POSIX;
