package body Hypervisor_Check is

    function CPUID (Arg : Unsigned_32) return CPUID_Registers is
        eax, ebx, ecx, edx : Unsigned_32;
    begin
	Asm("cpuid",
            Outputs => (Unsigned_32'Asm_Output ("=a", eax),
                        Unsigned_32'Asm_Output ("=b", ebx),
                        Unsigned_32'Asm_Output ("=c", ecx),
                        Unsigned_32'Asm_Output ("=d", edx)),
            Inputs  => Unsigned_32'Asm_Input ("a", Arg));
        return (eax, ebx, ecx, edx);
    end CPUID;

    function String_of_U32 (Arg : Unsigned_32) return Unbounded_String is
        Word : Unsigned_32;
        Result : Unbounded_String;
    begin
        Word := Arg;
        while Word > 0 loop
            Append (Result, Character'Val (Word and 16#FF#));
            Word := Shift_Right (Word, 8);
        end loop;
        return Result;
    end String_of_U32;

    function Get_Vendor_String return Unbounded_String is
        Vendor_String : Unbounded_String;
        Registers : CPUID_Registers;
    begin
        Registers := CPUID (Hypervisor_Leaf);
        Vendor_String := String_of_U32 (Registers(2)) &
                         String_of_U32 (Registers(3)) &
                         String_of_U32 (Registers(4));
        return Vendor_String;
    end Get_Vendor_String;

end Hypervisor_Check;
