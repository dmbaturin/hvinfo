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

    -- Xen support hardware and paravirtual modes, in paravirtual mode
    -- it's not detectable with CPUID
    function Xen_Present return Boolean is
    begin
        if Config.Linux then
            if Contains(Head_Of_File(Linux_Sys_HV_Type_File), "xen") then
                return True;
            else
                return False;
            end if;
        elsif Config.FreeBSD then
            return Command_Succeeds(Interfaces.C.To_C(FreeBSD_Xen_Present_Command));
        else
            raise OS_Not_Supported;
        end if;
    end Xen_Present;
 
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

    -- Execute a system command and return true if it succeeded
    -- (i.e. returned 0)
    function Command_Succeeds (Command : Interfaces.C.Char_Array) return Boolean is
        use Interfaces.C;
        function Sys (Arg : Char_Array) return Integer;
        pragma Import(C, Sys, "system");

        Ret_Val : Integer;
    begin
        Ret_Val := Sys(Command);
        if Ret_Val > 0 then
            return False;
        else
            return True;
        end if;
    end Command_Succeeds;

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
        elsif Vendor_String = "bhyve bhyve " then
            Vendor_Name := To_Unbounded_String (bhyve);
        else
            Vendor_Name := To_Unbounded_String  ("Unknown hypervisor");
        end if;
        return Vendor_Name;
    end Get_Vendor_Name;

    -- There are two cases: 1. DMI is not available on a _system_
    -- (paravirtualized guests are notable examples)
    -- 2. the OS doesn't have a DMI API available to unprivileged users
    function DMI_Available return Boolean is
    begin
        if Config.Linux then
            -- Linux provides DMI info via sysfs, but on systems
            -- without SMBIOS it's not available
            if Ada.Directories.Exists("/sys/class/dmi") then
                return True;
            else
                return False;
            end if;
        else
            -- Other OSes don't have DMI API we can use
            return False;
        end if;
    end DMI_Available;

    function Get_DMI_Vendor_String return US.Unbounded_String is
        Name : US.Unbounded_String;
    begin
        if Config.Linux then
            Name := Head_Of_File (Linux_Sys_Vendor_File);
        else
            raise OS_Not_Supported;
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

    function Known_DMI_HV_Vendor (Name : US.Unbounded_String) return Boolean is
        use US;
    begin
        if Name /= US.To_Unbounded_String("") then
            return True;
        else
            return False;
        end if;
    end Known_DMI_HV_Vendor;

end Hypervisor_Check;
