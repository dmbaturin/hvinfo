# hvinfo
Yet another hypervisor detection tool, this time in a high level language and
doesn't want root privileges.

To build it, you need GNAT 4.x or higher (Ada 2005 support required).
Just use "make" in the top level dir.
Alternatively, you can use gnatmake directly, from the top level dir do

```
gnatmake ./src/hvinfo
```

# OS and hypervisor support

CPUID-based checks that can detext KVM, Xen HVM, VMware, and Hyper-V are
supported on any OS.

Xen PV vs. Xen HVM check is supported on Linux and FreeBSD.

The easiest way to detect VirtualBox, Parallels, and some other hypervisors
that don't use CPUID identification is to check SMBIOS vendor name,
and this is currently only supported on Linux because it provides DMI information
via sysfs.

Container systems such as LXC and OpenVZ are not supported yet.

Virtualization systems for platforms other than x86 aren't supported either.

Patches are welcome.
