with Ada.Text_IO;     use Ada.Text_IO;

package body User_Error_Messages is
   
   procedure Warning ( Message : in String ) is
   begin
      Put_Line (Standard_Error, Command_Name &
                  ": WARNING, " & Message);
   end;
   
   procedure Error ( Message : in String; 
                     Status : Error_Status_Type ) is
   begin
      Put_Line (Standard_Error, Command_Name &
                  ": ERROR, " & Message);
      Ada.Command_Line.Set_Exit_Status (Status);
   end;
   
end User_Error_Messages;
