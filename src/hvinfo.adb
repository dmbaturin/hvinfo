------------------------------------------------------------------------
-- Copyright (C) 2018 Daniil Baturin <daniil@baturin.org>
--
-- This file is part of hvinfo.
--
-- hvinfo is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.
--
-- hvinfo is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with hvinfo.  If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------

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

    SMBIOS_Vendor : US.Unbounded_String;

    Debug : Boolean;
    Hypervisor_Detected : Boolean := False;
    Hypervisor_Name : US.Unbounded_String := US.Null_Unbounded_String;
begin
    -- Handle command line options
    declare
        -- No declarations
    begin
        loop
            case GCL.Getopt ("-help -version -debug") is
                when '-' =>
                    if GCL.Full_Switch = "-version" then
                        Print_Version;
                        return;
                    elsif GCL.Full_Switch = "-help" then
                        Print_Help;
                        return;
                    elsif GCL.Full_Switch = "-debug" then
                        Debug := True;
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

    -- Check for Xen first, as it has two distinct modes
    if Xen_Present then
        Hypervisor_Detected := True;

        if Hypervisor_Present then
            -- This is Xen HVM
            Hypervisor_Name := US.To_Unbounded_String (Xen_HVM);
        else
            -- Xen present and no CPUID leaf means Xen PV
            Hypervisor_Name := US.To_Unbounded_String (Xen_PV);
        end if;
    elsif Hypervisor_Present then
        -- This covers KVM, VMware, and other hypervisors
        -- that use CPUID leaf as their primary identification method

        Hypervisor_Detected := True;

        declare
            use US;
        begin
            if Debug then
                IO.Put_Line ("CPUID hypervisor bit is set");
                UIO.Put_Line ("Hypervisor identifier is """ & Get_Vendor_String & """");
            end if;

            Hypervisor_Name := Get_Vendor_Name;

            -- VirtualBox may use KVM or Hyper-V as its backend,
            -- but still exposes its own graphics card so that setup can be detected
            if VirtualBox_PCI_Present then
                Hypervisor_Name := "VirtualBox (using " & Hypervisor_Name & ")";
            end if;
        end;
    elsif DMI_Available then
        -- VirtualBox, Parallels, and possible others only
        -- mark their presence by setting SMBIOS vendor string

        -- If the vendor name matches a known name associated
        -- with a hypervisor, print it.
        -- Sadly, this will give a wrong result on systems without
        -- DMI reading API accessible to unprivileged users
        

        declare
            use US;
            SMBIOS_Vendor, SMBIOS_HV_Name : US.Unbounded_String;
        begin
            SMBIOS_Vendor := Get_DMI_Vendor_String;
            SMBIOS_HV_Name := Get_DMI_Vendor_Name (SMBIOS_Vendor);

            if SMBIOS_HV_Name /= Null_Unbounded_String then
                Hypervisor_Name := SMBIOS_HV_Name;
                Hypervisor_Detected := True;
            else
                if Debug then
                    UIO.Put_Line (IO.Standard_Error, "DMI vendor name is: """ & SMBIOS_Vendor & """");
                end if;
            end if;
         end;
    elsif VirtualBox_PCI_Present then
        Hypervisor_Name := US.To_Unbounded_String (VirtualBox);
    else
        if Debug then
            IO.Put_Line (IO.Standard_Error, "DMI reading API is not available to unpriviliged users on this OS");
        end if;
    end if;

    if Hypervisor_Detected then
        CL.Set_Exit_Status (0);
        UIO.Put_Line (Hypervisor_Name);
    else
        CL.Set_Exit_Status (1);
        IO.Put_Line (IO.Standard_Error, "No hypervisor detected");
    end if;

end HVInfo;

