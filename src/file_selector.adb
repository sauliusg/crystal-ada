with Text_Io;                use Text_Io;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with Ada.Unchecked_Deallocation;

package body File_Selector is
   
   -- Selects an STDIN for the input file ('Current_File'), or a file
   --  named by 'File_Name', and opens that file.
   
   procedure Select_File ( File_Name : in out Unbounded_String;
                           Is_File_Processed : in out Boolean;
                           Is_Last_File : out Boolean;
                           Current_File : out Access_File_Type ) is
   begin
      Is_Last_File := False;
      -- Put_Line ("Current file name: '" & To_String(File_Name) & "'");
      if File_Name = To_Unbounded_String ("") then
         if not Is_File_Processed then
            File_Name := To_Unbounded_String ("-");
            Current_File := new File_Type'(Standard_Input);
         else
            Is_Last_File := True;
         end if;
      else
         if File_Name = To_Unbounded_String ("-") then
            Current_File := new File_Type'(Standard_Input);
         else 
            Current_File := new File_Type;
            Open (Current_File.all, In_File, To_String(File_Name));
         end if;
      end if;
      Is_File_Processed := True;
   end;
         
end File_Selector;
