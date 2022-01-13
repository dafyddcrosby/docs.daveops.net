# C

## Install tooling in Fedora

```bash
sudo yum groupinstall "C Development Tools and Libraries"
```

## Resources

* <http://splint.org>
* <http://c-faq.com/>
* <https://www.securecoding.cert.org/confluence/display/c/SEI+CERT+C+Coding+Standard>
* [GCC non-bugs](https://gcc.gnu.org/bugs/#nonbugs_c)_
* <http://www.slideshare.net/olvemaudal/deep-c/24-What_will_happen_if_you>

## Misc Notes

* Don't cast returned pointers from malloc. (void \*) should get automatically promoted to any pointer type, and casting just makes it likely you'll get it wrong.
* Free allocated memory when you are done with it, don't assume that OS will clean up your mess.

## To verify

* ☐ C compiler will create implicit declaration of function, which can be satisfied at link time
* ☐ Difference between exit values in ANSI C, K+R, C99
* ☐ Are return values typically passed in a register?

## Open and read file

```c
const char *filename = "file.txt";
unsigned char byte;
FILE *fp;
 
fp = fopen(filename, "rb");
 
if (!fp) {
        printf("Couldn't open file\n");
        return 1;
}
 
while(!feof(fp)) {
        fread(&byte, sizeof(int), 1, fp);
        printf("%i\n",byte);
}
 
fclose(fp);
```


# glibc

## Get version

```bash
/lib/libc.so.6
```

or

```c
#include <stdio.h>
#include <gnu/libc-version.h>
int main (void) { puts (gnu_get_libc_version ()); return 0; }
```
