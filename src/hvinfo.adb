with Interfaces; use Interfaces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Hypervisor_Check; use Hypervisor_Check;

procedure HVInfo is
begin

    Put_Line (To_String (Get_Vendor_String));

end HVInfo;

