with Text_Io;                use Text_Io;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with Ada.Unchecked_Deallocation;

package File_Name_Selector is
   
   type Access_File_Type is access File_Type;

   procedure Free_File is
      new Ada.Unchecked_Deallocation(File_Type, Access_File_Type);
   
   procedure Select_File ( File_Name : in out Unbounded_String;
                           Is_File_Processed : in out Boolean;
                           Is_Last_File : out Boolean;
                           Current_File : out Access_File_Type );

end File_Name_Selector;
