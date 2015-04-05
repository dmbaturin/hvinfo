package body HVInfo_Util is

    procedure Print_Help is
    begin
        IO.Put_Line ("hvinfo is a hypervisor detection tool");
        IO.Put_Line ("Usage: hvinfo [--help | --version]");
        IO.Put_Line ("");
        IO.Put_Line ("  --help     Print this message and exit");
        IO.Put_Line ("  --version  Print version and exit");
    end Print_Help;

    procedure Print_Version is
    begin
        IO.Put_Line ("hvinfo " & Config.Version);
        IO.Put_Line ("Copyright 2015 Daniil Baturin <daniil@baturin.org>");
        IO.Put_Line ("");
        IO.Put_Line ("This program is free software, distributed under the terms");
        IO.Put_Line ("of the GNU General Public License version 2 or later.");
    end Print_Version;

end HVInfo_Util;
