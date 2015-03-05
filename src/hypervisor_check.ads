with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Hypervisor_Check is

    Hypervisor_Leaf : constant := 16#40000000#;
    type CPUID_Registers is array (1 .. 4) of Unsigned_32;

    function CPUID (Arg : Unsigned_32) return CPUID_Registers;

    function String_of_U32 (Arg : Unsigned_32) return Unbounded_String;

    function Get_Vendor_String return Unbounded_String;

end Hypervisor_Check;
