// How System.Native's SupportsCopyFileRange reads a kernel release:
// sscanf(release, "%u.%u", &major, &minor) into two zeroes, copy_file_range
// used from 5.3. Run with glibc 2.41 (gcc:14 in `container`) on 2026-10-01:
//
// container run --rm -v "$PWD":/p gcc:14 sh -c 'gcc -w -o /tmp/s /p/release-sscanf.c && /tmp/s'
//
//   '5.3.0' -> 5.3 supported=1
//   '5.3' -> 5.3 supported=1
//   '6.0' -> 6.0 supported=1
//   '10.1-rc1' -> 10.1 supported=1
//   '5.+3' -> 5.3 supported=1
//   '5. 3' -> 5.3 supported=1
//   ' 5.3' -> 5.3 supported=1
//   '-1.0' -> 4294967295.0 supported=1
//   '5.-3' -> 5.4294967293 supported=1
//   '5.2.99' -> 5.2 supported=0
//   '5' -> 5.0 supported=0
//   '4.19.0' -> 4.19 supported=0
//   'x5.3' -> 0.0 supported=0
//   '5 .3' -> 5.0 supported=0
//   '99999999999999999999.1' -> 4294967295.1 supported=1
//   '5.-18446744073709551616' -> 5.4294967295 supported=1
//   '5.-18446744073709551615' -> 5.1 supported=0
//   '5.-4294967293' -> 5.3 supported=1
#include <stdio.h>
int main(void) {
    const char *r[] = { "5.3.0", "5.3", "6.0", "10.1-rc1", "5.+3", "5. 3", " 5.3", "-1.0", "5.-3", "5.2.99", "5", "4.19.0", "x5.3", "5 .3", "99999999999999999999.1", "5.-18446744073709551616", "5.-18446744073709551615", "5.-4294967293" };
    for (int i = 0; i < 18; i++) {
        unsigned int major = 0, minor = 0;
        sscanf(r[i], "%u.%u", &major, &minor);
        printf("'%s' -> %u.%u supported=%d\n", r[i], major, minor, major > 5 || (major == 5 && minor >= 3));
    }
    return 0;
}
