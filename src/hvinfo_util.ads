with Ada.Command_line;
with Ada.Text_IO;

with Config;

package HVInfo_Util is

    package IO renames Ada.Text_IO;

    procedure Print_Version;
    procedure Print_Help;

end HVINfo_Util;
