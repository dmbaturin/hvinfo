------------------------------------------------------------------------
-- Copyright (C) 2015 Daniil Baturin <daniil@baturin.org>
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

    SMBIOS_HV_Name : US.Unbounded_String;
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
            IO.Put_Line (Xen_HVM);
        else
            -- Xen present and no CPUID leaf means Xen PV
            IO.Put_Line (Xen_PV);
        end if;
    elsif Hypervisor_Present then
        -- This covers KVM, VMware, and other hypervisors
        -- that use CPUID leaf as primary identification method
        UIO.Put_Line (Get_Vendor_Name);
    else
        -- VirtualBox, Parallels, and possible others only
        -- mark their presence by setting SMBIOS vendor string
        if DMI_Available then
            -- If the vendor name matches a known name associated
            -- with a hypervisor, print it.
            -- Sadly, this will give a wrong result on systems without
            -- DMI reading API accessible to unprivileged users
            SMBIOS_HV_Name := Get_DMI_Vendor_Name;
            if Known_DMI_HV_Vendor (SMBIOS_HV_Name) then
                UIO.Put_Line (SMBIOS_HV_Name);
            else
                IO.Put_Line (IO.Standard_Error, "No hypervisor detected");
                CL.Set_Exit_Status (1);
            end if;
        elsif VirtualBox_PCI_Present then
            IO.Put_Line (VirtualBox);
        end if;
    end if;

end HVInfo;

