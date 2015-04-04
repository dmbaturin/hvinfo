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

    -- Convert an unsigned 32-bit integer to a string of 4 characters
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

    function Contains (Haystack : US.Unbounded_String; Needle : String)
    return Boolean is
        Position : Natural;
    begin
        Position := US.Index (Source => Haystack, Pattern => Needle);
        if Position > 0 then
            return True;
        else
            return False;
        end if;
    end Contains;

    -- Read the first line of file, return empty string in case of errors
    -- Pretty much what we need for reading /proc files etc.
    function Head_Of_File (Path : String) return US.Unbounded_String is
        File : IO.File_Type;
        Result : US.Unbounded_String;
    begin
        IO.Open(File => File, Name => Path, Mode => IO.In_File);
        UIO.Get_Line (File, Result);
        IO.Close(File);
        return Result;
    exception
        when others => return US.To_Unbounded_String ("");
    end Head_Of_File;
 
    -- Hypervisors should set the bit 31 of %ecx to 1 in CPUID leaf 1
    function Hypervisor_Present return Boolean is
        Registers : CPUID_Registers;
    begin
        Registers := CPUID (1);
        if (((Shift_Right (Registers(3), 31)) and 1) = 1) then
            return True;
        else
            return False;
        end if;
    end Hypervisor_Present;

    -- Calling CPUID instruction with hypervisor leaf in %eax
    -- puts the vendor string in %ebx, %ecx, and %edx
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

    function Get_DMI_Vendor_String return US.Unbounded_String is
        Name : US.Unbounded_String;
    begin
        if Config.Linux then
            Name := Head_Of_File (Linux_Sys_Vendor_File);
        else
            Name := US.To_Unbounded_String("");
        end if;

        return Name;
    end Get_DMI_Vendor_String;

    function Get_DMI_Vendor_Name return US.Unbounded_String is
        Vendor_String, Vendor_Name : US.Unbounded_String;
    begin
        Vendor_String := Get_DMI_Vendor_String;
        if Contains(Vendor_String, VMWare_DMI_Pattern) then
            Vendor_Name := US.To_Unbounded_String (VMWare);
        elsif Contains(Vendor_String, HyperV_DMI_Pattern) then
            Vendor_Name := US.To_Unbounded_String (HyperV);
        elsif Contains(Vendor_String, VirtualBox_DMI_Pattern) then
            Vendor_Name := US.To_Unbounded_String (VirtualBox);
        elsif Contains(Vendor_String, Parallels_DMI_Pattern) then
            Vendor_Name := US.To_Unbounded_String (Parallels);
        else
            Vendor_Name := US.To_Unbounded_String ("");
        end if;
        return Vendor_Name;
    end Get_DMI_Vendor_Name;

end Hypervisor_Check;
