---
title: Bash
---

## Go immediately to $EDITOR

<kbd>ctrl</kbd>-x, e

## Most commonly used commands

```bash
history | awk '{a[$2]++}END{for(i in a){print a[i] " " i}}' | sort -rn | head
```

## Delete files that are not extension

```bash
rm !(*.foo|*.bar|*.baz)
```

## Find, delete empty directories

```bash
find . -type d -empty -exec rmdir {} \;
```

## Show your shell from a port

```bash
script -qf | tee >(nc -kl 5000) >(nc -kl 5001) >(nc -kl 5002)
```

## Replace filename spaces with underscores

```bash
# util-linux-ng
rename " " _ *
```

## Search for Unicode use in a tree

TODO - I think this could be done with one 'find' command, no need to loop...

```bash
for FILE in $(find . -type f) ; do echo File: ${FILE}; perl -ane '{ if(m/[[:^ascii:]]/) {print  } } ' ${FILE}; done
```

## "Press any key to continue"

```bash
read -sn 1 -p "Press any key to continue..."
```

## Conditional Expressions

code      | desc
---       | ---
-a file   | file exists
-d file   | file exists and is a directory
-r file   | file exists and is readable
-w file   | file exists and is writeable
-x file   | file exists and is executable
-z string | true if length of string is zero

## Process Substitution

A temporary named pipe

```bash
diff <(grep lines file1) <(grep lines file2)
thing --output >(gzip > output.txt.gz)
```

## Syntax cheatsheet

```bash
fun () { echo "totes a function"; exit 1 ; } #Don't forget trailing colon if one line

if [ -e file ] ; then
	echo "file exists"
fi

case expression in
       pattern1 )
       	statements ;;
       pattern2 )
       	statements ;;
       ...
esac

# Bash 4+
# for VARIABLE in 1 2 3 4 5
for VARIABLE in {1..5}
do
       	command1
       	command2
       	commandN
done
# to do stepping, use {1..99..2}
```

## set

<https://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html>

flag        | desc
---         | ---
-e          | exit if a pipeline returns non-zero
-o pipefail | return value of a pipeline is the value of the last (rightmost) command to exit with a non-zero status
-o posix    | match POSIX standard behaviour (<https://www.gnu.org/software/bash/manual/html_node/Bash-POSIX-Mode.html#Bash-POSIX-Mode>)
-n          | read commands but do not execute (used for checking syntax)
-u          | treat unset variables and parameters as an error when performing parameter expansion
-x          | print trace of commands as they are executed
-C          | prevent output redirection using ‘>’, ‘>&’, and ‘<>’ from overwriting existing files


## Using regex for variable testing

```bash
if [[ $HOSTNAME =~ host[0-9].example.com ]]; then
    echo "yay"
fi
```

## Temporary directory/file

```bash
mktemp -d
```

## Variables / functions

```bash
# Set an environment variable
declare -x BLARG=5
# Show the functions declared in the shell
declare -F
# show functions on ancient shells
typeset -F
```

## Use heredocs

```bash
cat <<EOM > file.out
blah
blah
EOM
```

## Quit without saving history

```bash
unset HISFILE && exit
```

## Regex change over files returned from grep

```bash
# macOS
grep -l ... | xargs -I% sed -i".bkp" -e "s/old/new/" %
```

## I/O Redirection

|      | description                     |
|:-----|:--------------------------------|
| &>   | Redirect both stderr and stdout |
| 1>   | Redirect stdout                 |
| 2>   | Redirect stderr                 |
| 2>&1 | Redirect stderr to stdout       |
| <    | Redirect stdin to a process     |

