# hvinfo
Yet another hypervisor detection tool, this time in a high level language and
doesn't want root privileges.

To build it, you need GNAT 4.x or higher (Ada 2005 support required).
Just use "make" in the top level dir.

Currently CPUID-based checks that can detect KVM, Xen, VMWare, and Hyper-V
are supported on any OS, but SMBIOS vendor checks are Linux-specific.
If you know how to extend them to other OSes, patches are welcome
(as long as it doesn't require root privileges, as it kinda was the motivation).
