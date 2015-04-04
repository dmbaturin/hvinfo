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

    -- Assume success until proven otherwise
    CL.Set_Exit_Status (0);


    -- Check for Xen first, as it has two distinct modes
    if Xen_Present then
        if Hypervisor_Present then
            -- This is Xen HVM
            IO.Put_Line(Xen_HVM);
        else
            -- Xen present and no CPUID leaf means Xen PV
            IO.Put_Line(Xen_PV);
        end if;
    elsif Hypervisor_Present then
        -- This covers KVM, VMware, and other hypervisors
        -- that use CPUID leaf as primary identification method
        UIO.Put_Line (CPUID_HV_Name);
    else
        -- VirtualBox, Parallels, and possible others only
        -- mark their presence by setting SMBIOS vendor string
        if DMI_Available then
            -- If the vendor name matches a known name associated
            -- with a hypervisor, print it.
            -- Sadly, this will give a wrong result on systems without
            -- DMI reading API accessible to unprivileged users
            SMBIOS_HV_Name := Get_DMI_Vendor_Name;
            if Known_DMI_HV_Vendor(SMBIOS_HV_Name) then
                UIO.Put_Line (SMBIOS_HV_Name);
            else
                IO.Put_Line(IO.Standard_Error, "No hypervisor detected");
                CL.Set_Exit_Status (1);
            end if;
        end if;
    end if;

end HVInfo;

