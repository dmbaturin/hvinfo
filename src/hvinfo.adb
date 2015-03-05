with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Hypervisor_Check; use Hypervisor_Check;
procedure HVInfo is
begin

    -- Try CPUID checks first
    if Hypervisor_Present then
        Put_Line (Get_Vendor_Name);
        Set_Exit_Status (0);
    else
        Set_Exit_Status (1);
    end if;
end HVInfo;

