with Ada.Command_Line;      use Ada.Command_Line;

package User_Error_Messages is
   
   subtype Error_Status_Type is Exit_Status range 1..255;
   
   procedure Warning ( Message : in String );
   
   procedure Error ( Message : in String; Status : Error_Status_Type );

end User_Error_Messages;
