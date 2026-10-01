# Runs the File.Copy guests (sourcesPure/CopyFileSeeded.cs and
# CopyFileUnauthorized.cs, sourcesImpure/CopyFileWiringLinuxSeeded.cs) on real
# .NET on Linux, each built as a net10.0 console app into out/<name>, in a tree
# laid out as its seed, as uid 1000 (and under umask 077 for the wiring guest):
#
# container run --rm -v "$PWD":/g mcr.microsoft.com/dotnet/runtime:10.0.7 sh /g/copy-file-guests-linux.sh
#
# On Linux 6.18.5 aarch64 on 2026-10-01 every guest exited 0, on tmpfs and on
# ext4. The Darwin wiring guest exits 0 on Darwin 27.0 in the same kind of tree.
set -u
for fs in /dev/shm /tmp; do
echo "=== $fs"
# The pure guest, in a tree laid out as its seed, as uid 1000.
B=$fs/pure; rm -rf $B; mkdir -p $B/d; cd $B
printf hello > f; chmod 640 f; printf written > m; printf w > w; chmod 666 w; printf 'previous content that is longer' > g; : > empty; printf held > held; ln -s f lf; printf in > d/in
chown -R 1000:1000 $B
setpriv --reuid 1000 --regid 1000 --clear-groups sh -c "cd $B && dotnet /g/out/CopyFileSeeded/CopyFileSeeded.dll"; echo "CopyFileSeeded exit $?"
B=$fs/unauth; rm -rf $B; mkdir -p $B; chown 1000:1000 $B
setpriv --reuid 1000 --regid 1000 --clear-groups sh -c "cd $B && dotnet /g/out/CopyFileUnauthorized/CopyFileUnauthorized.dll"; echo "CopyFileUnauthorized exit $?"
# The Linux wiring guest, as uid 1000 under umask 077.
B=$fs/wiring; rm -rf $B; mkdir -p $B/ro; cd $B
printf hello > src; : > empty; printf hello > f; chmod 640 f; printf hello > suid; chmod 4755 suid
printf previous > theirs; chmod 666 theirs
printf 'writable inside' > ro/w; chmod 666 ro/w
ln -s nowhere dang; ln -s t lt; printf target > t
chown -R 1000:1000 $B; chown 0:0 theirs; chmod 4755 suid; chmod 555 ro
ls -la $B $B/ro
setpriv --reuid 1000 --regid 1000 --clear-groups sh -c "umask 077; cd $B && dotnet /g/out/CopyFileWiringLinuxSeeded/CopyFileWiringLinuxSeeded.dll"; echo "CopyFileWiringLinuxSeeded exit $?"
done
