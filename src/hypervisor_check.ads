with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded;

package Hypervisor_Check is

    package US renames Ada.Strings.Unbounded;

    function Get_Vendor_Name return US.Unbounded_String;

    function Hypervisor_Present return Boolean;

private

    Hypervisor_Leaf : constant := 16#40000000#;
    type CPUID_Registers is array (1 .. 4) of Unsigned_32;

    function CPUID (Arg : Unsigned_32) return CPUID_Registers;

    function String_of_U32 (Arg : Unsigned_32) return US.Unbounded_String;

    function Get_Vendor_String return US.Unbounded_String;

end Hypervisor_Check;
