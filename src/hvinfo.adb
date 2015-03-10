with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Hypervisor_Check; use Hypervisor_Check;

procedure HVInfo is
    package US renames Ada.Strings.Unbounded;
    package CL renames Ada.Command_Line;
    CPUID_HV_Name, SMBIOS_HV_Name, Hypervisor_Name : US.Unbounded_String;
begin
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

