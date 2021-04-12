---
title: Python
---

<!--
.. TODO - ConfigParser
.. TODO - gdchart
.. TODO - asyncore/asynchat
.. TODO - shlex
.. TODO - paramiko
.. TODO - threading, multiprocessing
-->

## Essential third-party tools

* [mypy](http://mypy-lang.org/)
* [autopep8](http://pypi.python.org/pypi/autopep8)

## Getopts

```python
import getopt, sys

try:
    opts, args = getopt.getopt(sys.argv[1:], 'eq', ["session=", "server="])
except getopt.GetoptError, err:
    # print help information and exit:
    print str(err)
    sys.exit(1)
for opt, val in opts:
    if opt in ('-e'):
        extend = True
    elif opt in ('-q'):
        query = True
    elif opt in ('--session'):
        session = val
    elif opt in ('--server'):
        server = val
```

## Opening a file

```python
try:
    with open(filepath, "r") as file_handle:
        for line in file_handle:
           ...
    except IOError, err:
        print(err)
```

## Syntax cheatsheet

```python
if blah == 0:
    print("0")
elif == 1:
    print("1")
else:
    print("else")

def printarg (arg):
    print(arg)

if __name__ == "__main__":
    main()
```

## String formatting

```python
user="world"

# Interpolated (3.6+)
print(f"hello {user}")
# New
print("hello {}".format(user))
# Old
print("hello %s" % user)
```

## Remove duplicates from a list

```python
the_list = list(set(the_list))
```

## Profile a program

<http://docs.python.org/library/profile.html>

```python
import profile
profile.run("main()")
```

## Launch REPL when line is hit

```python
import code
code.interact(local=locals())
```

## Logging

```python
import logging
logging.basicConfig(filename='myapp.log', level=logging.INFO)
logging.warning('%s before you %s', 'Look', 'leap!')
```

## Named tuples (ghetto classes)

```python
import collections

Prisoner = collections.namedtuple('Prisoner', 'name rank serial')

hogan = Person(name='Hogan', age='Colonel', serial='1234')
lebeau = Person(name='Lebeau', age='Private', serial='8888')
print 'Name:', lebeau.name

for prisoner in [ hogan, lebeau ]:
    print '%s is a %d, serial: %s' % prisoner
```

## Tab completion in Python shell

If you don't have access to IPython,

```python
import rlcompleter, readline
readline.parse_and_bind('tab: complete')
```

## Tar a bunch of files

```python
import tarfile
tar = tarfile.open("sample.tar", "w")
for name in ["foo", "bar", "quux"]:
tar.add(name)
tar.close()
```

## Run a simple webserver

```bash
python -m SimpleHTTPServer
```

## Check Python version

```python
 if sys.hexversion >= 0x020502F0:
 # use some advanced feature
 ...
 else:
 # use an alternative implementation or warn the user
 ...
```

bit   | description
---   | ---
1-8   | PY_MAJOR_VERSION (the 2 in 2.1.0a3)
9-16  | PY_MINOR_VERSION (the 1 in 2.1.0a3)
17-24 | PY_MICRO_VERSION (the 0 in 2.1.0a3)
25-28 | PY_RELEASE_LEVEL (0xA for alpha, 0xB for beta, 0xC for release candidate and 0xF for final)
29-32 | PY_RELEASE_SERIAL (the 3 in 2.1.0a3, zero for final releases)

## Get Linux distribution

```python
if sys.hexversion < 0x020600F0:
platform.dist()
else:
platform.linux_distribution()
```

## Inspect the stack

```python
import inspect
print " << ".join([i[3] for i in inspect.stack()])
```

## Get Python documentation through the browser

```bash
pydoc -p $PORT
```

<!-- vim: set nospell: -->
