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

    function String_of_U32 (Arg : Unsigned_32) return US.Unbounded_String is
        Word : Unsigned_32;
        Result : US.Unbounded_String;
    begin
        Word := Arg;
        while Word > 0 loop
            US.Append (Result, Character'Val (Word and 16#FF#));
            Word := Shift_Right (Word, 8);
        end loop;
        return Result;
    end String_of_U32;

    -- Hypervisors should set the bit 31 of %ecx to 1 in CPUID leaf 1
    function Hypervisor_Present return Boolean is
        Registers : CPUID_Registers;
        ECX : Unsigned_32;
    begin
        Registers := CPUID (1);
        ECX := Shift_Right (Registers(3), 31);
        if (ecx and 1) = 1 then
            return True;
        else
            return False;
        end if;
    end Hypervisor_Present;

    function Get_Vendor_String return US.Unbounded_String is
        use US;
        Vendor_String : US.Unbounded_String;
        Registers : CPUID_Registers;
    begin
        Registers := CPUID (Hypervisor_Leaf);
        Vendor_String := String_of_U32 (Registers(2)) &
                         String_of_U32 (Registers(3)) &
                         String_of_U32 (Registers(4));
        return Vendor_String;
    end Get_Vendor_String;

    function Get_Vendor_Name return US.Unbounded_String is
        use US;
        Vendor_String, Vendor_Name : Unbounded_String;
    begin
        Vendor_String := Get_Vendor_String;
	if Vendor_String = "KVMKVMKVM" then
            Vendor_Name := To_Unbounded_String ("KVM");
        elsif Vendor_String = "XenVMMXenVMM" then
            Vendor_Name := To_Unbounded_String ("Xen");
        elsif Vendor_String = "VMwareVMware" then
            Vendor_Name := To_Unbounded_String ("VMWare");
        elsif Vendor_String = "Microsoft Hv" then
            Vendor_Name := To_Unbounded_String ("Microsoft Hyper-V");
        else
            Vendor_Name := To_Unbounded_String  ("Unknown hypervisor");
        end if;
        return Vendor_Name;
    end Get_Vendor_Name;


end Hypervisor_Check;