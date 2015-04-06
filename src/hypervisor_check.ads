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

with Interfaces; use Interfaces;
with Interfaces.C;
with System.Machine_Code; use System.Machine_Code;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Directories;

with Config;

package Hypervisor_Check is

    package IO renames Ada.Text_IO;
    package US renames Ada.Strings.Unbounded;
    package UIO renames Ada.Text_IO.Unbounded_IO;

    OS_Not_Supported : exception;

    function Get_Vendor_Name return US.Unbounded_String;

    function Hypervisor_Present return Boolean;

    function Xen_Present return Boolean;

    function DMI_Available return Boolean;

    function Get_DMI_Vendor_Name return US.Unbounded_String;

    function Known_DMI_HV_Vendor (Name : US.Unbounded_String) return Boolean;

    function Command_Succeeds (Command : Interfaces.C.Char_Array) return Boolean;

    function VirtualBox_PCI_Present return Boolean;

    -- Vendor names for human consumption
    VMWare : constant String := "VMWare";
    Xen_HVM : constant String := "Xen HVM";
    Xen_PV : constant String := "Xen PV";
    KVM : constant String := "KVM";
    HyperV : constant String := "Microsoft Hyper-V";
    VirtualBox : constant String := "VirtualBox";
    Parallels : constant String := "Parallels";
    bhyve : constant String := "bhyve";

private

    Hypervisor_Leaf : constant := 16#40000000#;
    type CPUID_Registers is array (1 .. 4) of Unsigned_32;

    -- Linux-specific file names etc.
    Linux_Sys_Vendor_File : constant String := "/sys/class/dmi/id/sys_vendor";
    Linux_Sys_HV_Type_File : constant String := "/sys/hypervisor/type";

    -- FreeBSD-specific file names, commands etc.

    -- sysctl read commands are available to unprivileged users, but sysctl binary
    -- may not be in the $PATH, hence the absolute path
    FreeBSD_Xen_Present_Command : constant String := "/sbin/sysctl kern.vm_guest | grep xen > /dev/null";

    -- SMBIOS vendor strings
    VMWare_DMI_Pattern : constant String := "VMware, Inc.";
    HyperV_DMI_Pattern : constant String := "Microsoft Corporation";
    VirtualBox_DMI_Pattern : constant String := "innotek GmbH";
    Parallels_DMI_Pattern : constant String := "Parallels";

    function CPUID (Arg : Unsigned_32) return CPUID_Registers;

    function String_of_U32 (Arg : Unsigned_32) return US.Unbounded_String;

    function Get_Vendor_String return US.Unbounded_String;

    function Head_Of_File (Path : String) return US.Unbounded_String;

    function Contains (Haystack : US.Unbounded_String; Needle : String) return Boolean;

    function Get_DMI_Vendor_String return US.Unbounded_String;

end Hypervisor_Check;
