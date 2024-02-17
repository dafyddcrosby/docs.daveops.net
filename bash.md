# Bash

You should try to stay in bash as long as possible without dropping into a subshell (ie running another program).

- <https://github.com/dylanaraps/pure-bash-bible>
- <https://github.com/dylanaraps/pure-sh-bible>


## Go immediately to $EDITOR

<kbd>ctrl</kbd>-x, e


## Most commonly used commands

```shell
history | awk '{a[$2]++}END{for(i in a){print a[i] " " i}}' | sort -rn | head
```


## Delete files that are not extension

```shell
rm !(*.foo|*.bar|*.baz)
```


## Find, delete empty directories

```shell
find . -type d -empty -exec rmdir {} \;
```


## Show your shell from a port

```shell
script -qf | tee >(nc -kl 5000) >(nc -kl 5001) >(nc -kl 5002)
```


## Replace filename spaces with underscores

```shell
# util-linux-ng
rename " " _ *
```


## Search for Unicode use in a tree

```shell
for FILE in $(find . -type f) ; do echo File: ${FILE}; perl -ane '{ if(m/[[:^ascii:]]/) {print  } } ' ${FILE}; done
```


## "Press any key to continue"

```shell
read -sn 1 -p "Press any key to continue..."
```


## Arrays

| code           | desc                                    |
|-------------- |--------------------------------------- |
| arr=()         | Create an empty array                   |
| arr=(1 2 3)    | Initialize array                        |
| ${arr[N]}      | Retrieve Nth element                    |
| ${arr[@]}      | Retrieve all elements                   |
| ${!arr[@]}     | Retrieve array indices                  |
| ${#arr[@]}     | Calculate array size                    |
| arr[N]=foo     | Overwrite Nth element                   |
| arr+=(bar)     | Append value 'bar'                      |
| arr=( $(cmd) ) | Save `cmd` output as an array           |
| unset arr[N]   | Delete element at N                     |
| ${arr[@]:x:n}  | Retrieve n elements starting at index x |
| read -a arr    | Read STDIN as an array to arr variable  |


## Conditional Expressions

| code      | desc                                 |
|--------- |------------------------------------ |
| -a file   | file exists                          |
| -d file   | file exists and is a directory       |
| -h file   | file exists and is a symbolic link   |
| -r file   | file exists and is readable          |
| -w file   | file exists and is writeable         |
| -x file   | file exists and is executable        |
| -S file   | file exists and is a socket          |
| -n string | true if length of string is non-zero |
| -z string | true if length of string is zero     |

- [[Bash manual] Conditional Expressions](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [[Bash manual] Conditional Constructs](https://www.gnu.org/software/bash/manual/html_node/Conditional-Constructs.html)


## Process Substitution

A temporary named pipe

```shell
diff <(grep lines file1) <(grep lines file2)
thing --output >(gzip > output.txt.gz)
```


## Syntax cheatsheet

```shell
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

[[Bash manual] set builtin](https://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html)

| flag        | desc                                                                                                                       |
|----------- |-------------------------------------------------------------------------------------------------------------------------- |
| -e          | exit if a pipeline returns non-zero                                                                                        |
| -o pipefail | return value of a pipeline is the value of the last (rightmost) command to exit with a non-zero status                     |
| -o posix    | match POSIX standard behaviour (<https://www.gnu.org/software/bash/manual/html_node/Bash-POSIX-Mode.html#Bash-POSIX-Mode>) |
| -n          | read commands but do not execute (used for checking syntax)                                                                |
| -u          | treat unset variables and parameters as an error when performing parameter expansion                                       |
| -x          | print trace of commands as they are executed                                                                               |
| -C          | prevent output redirection using '>', '>&', and '<>' from overwriting existing files                                       |


## Using regex for variable testing

```shell
if [[ $HOSTNAME =~ host[0-9].example.com ]]; then
    echo "yay"
fi
```


## Temporary directory/file

```shell
mktemp -d
```


## Variables / functions

```shell
# Set an environment variable
declare -x BLARG=5
# Show the functions declared in the shell
declare -F
# show functions on ancient shells
typeset -F
```


## Use heredocs

```shell
cat <<EOM > file.out
blah
blah
EOM
```


## Quit without saving history

```shell
unset HISFILE && exit
```


## Regex change over files returned from grep

```shell
# macOS
grep -l ... | xargs -I% sed -i".bkp" -e "s/old/new/" %
```


## I/O Redirection

| code | description                     |
|---- |------------------------------- |
| &>   | Redirect both stderr and stdout |
| 1>   | Redirect stdout                 |
| 2>   | Redirect stderr                 |
| 2>&1 | Redirect stderr to stdout       |
| <    | Redirect stdin to a process     |
