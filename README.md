# hvinfo
Yet another hypervisor detection tool, this time in a high level language and
doesn't want root privileges.

## Building

To build it, you need GNU Make, GNAT 4.x or higher (Ada 2005 support required) and gprbuild.

Build setup is rather simplistic at this time.
```
cd hvinfo
gmake
gmake install PREFIX=<some dir>
```

### Building a Debian package

Debian packaging is not yet good enough to get it included in Debian, but should
be fine for local use. Install the devscripts package and do:

```
debuild -us -uc
```

If you have your Ada toolchain installed in an unusual location, it's better to ensure you have it
in your $PATH.

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

# TODO
* Implementation-independent makefile (anyone knows how to make ifdef work in both make flavours?)
* Containers virtualization detection.
