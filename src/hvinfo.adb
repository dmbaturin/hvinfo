with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;
with GNAT.Command_Line;
with GNAT.Strings;

with Hypervisor_Check; use Hypervisor_Check;
with HVInfo_Util; use HVInfo_Util;
with Config;

procedure HVInfo is
    package US renames Ada.Strings.Unbounded;
    package CL renames Ada.Command_Line;
    package IO renames Ada.Text_IO;
    package GCL renames GNAT.Command_Line;

    CPUID_HV_Name, SMBIOS_HV_Name, Hypervisor_Name : US.Unbounded_String;
begin
    -- Handle command line options
    declare
        -- No declarations
    begin
        loop
            case GCL.Getopt ("-help -version") is
                when '-' =>
                    if GCL.Full_Switch = "-version" then
                        Print_Version;
                        return;
                    elsif GCL.Full_Switch = "-help" then
                        Print_Help;
                        return;
                    end if;
                when others =>
                    exit;
            end case;
        end loop;
     exception
         when GCL.Invalid_Switch =>
             IO.Put_Line ("Invalid option");
             Print_Help;
             return;
     end;

    CPUID_HV_Name := US.To_Unbounded_String ("");
    SMBIOS_HV_Name := US.To_Unbounded_String ("");

    if Hypervisor_Present then
        CPUID_HV_Name := Get_Vendor_Name;
    end if;

    SMBIOS_HV_Name := Get_DMI_Vendor_Name;

    declare
        use US;
    begin
        if (CPUID_HV_Name = "") and (SMBIOS_HV_Name = "") then
            CL.Set_Exit_Status (1);
        elsif (CPUID_HV_Name /= "") then
            CL.Set_Exit_Status (0);
            UIO.Put_Line (CPUID_HV_Name);
        else
            CL.Set_Exit_Status (0);
            UIO.Put_Line (SMBIOS_HV_Name); 
        end if;
    end;

end HVInfo;

