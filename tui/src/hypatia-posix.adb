--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------
--  Hypatia.POSIX - Implementation
-------------------------------------------------------------------------------

package body Hypatia.POSIX is

   ---------------------------------------------------------------------------
   --  fd_set operations (bit manipulation)
   ---------------------------------------------------------------------------

   Bits_Per_Word : constant := unsigned_long'Size;

   --  Forward declaration
   function Bit_Mask (Bit_Index : Natural) return unsigned_long;

   --  Shift left using 2**Bit_Index
   function Bit_Mask (Bit_Index : Natural) return unsigned_long is
      Result : unsigned_long := 1;
   begin
      for I in 1 .. Bit_Index loop
         Result := Result * 2;
      end loop;
      return Result;
   end Bit_Mask;

   procedure Clear_FD_Set (fds : out FD_Set_Type) is
   begin
      fds := [others => 0];
   end Clear_FD_Set;

   procedure Set_FD (fd : int; fds : in out FD_Set_Type) is
      Word_Index : constant Natural := Natural (fd) / Bits_Per_Word;
      Bit_Index  : constant Natural := Natural (fd) mod Bits_Per_Word;
   begin
      if Word_Index <= fds'Last then
         fds (Word_Index) := fds (Word_Index) or Bit_Mask (Bit_Index);
      end if;
   end Set_FD;

   function Is_FD_Set (fd : int; fds : FD_Set_Type) return Boolean is
      Word_Index : constant Natural := Natural (fd) / Bits_Per_Word;
      Bit_Index  : constant Natural := Natural (fd) mod Bits_Per_Word;
   begin
      if Word_Index <= fds'Last then
         return (fds (Word_Index) and Bit_Mask (Bit_Index)) /= 0;
      else
         return False;
      end if;
   end Is_FD_Set;

end Hypatia.POSIX;
