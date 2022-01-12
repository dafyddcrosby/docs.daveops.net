---
title: Python
---

<!--
TODO - ConfigParser
TODO - gdchart
TODO - asyncore/asynchat
TODO - shlex
TODO - paramiko
TODO - threading, multiprocessing
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



## Disassemble a code object

```python
import dis
dis.dis(func)
```

<http://akaptur.com/blog/2013/08/14/python-bytecode-fun-with-dis/>





## Create virtual environment

```bash
python3 -m venv path/to/new/env
```


Python zen
----------

```python
import this
```


## enums

<!--
http://xion.io/post/code/python-enums-are-ok.html
https://docs.python.org/3/library/enum.html#module-enum
-->

```python
from enum import Enum
class Color(Enum):
    red = 1
    green = 2
    blue = 3
```


# platform

```python
# Get machine type
platform.machine()
# Determine system type (e.g. Linux, Windows)
platform.system()
# Get Linux distro
platform.linux_distribution()
```


# unittest
Up to 2.7, use ``unittest2`` since it's got more assert tests than ``unittest`` (they were all backported from the 2.7 release).

## Running tests

```bash
python -m unittest discover -s project_directory -p '*_test.py'
```

## Example

```python
import random
import unittest

class TestSequenceFunctions(unittest.TestCase):

def setUp(self):
    self.seq = range(10)

def test_shuffle(self):
    # make sure the shuffled sequence does not lose any elements
    random.shuffle(self.seq)
    self.seq.sort()
    self.assertEqual(self.seq, range(10))

# should raise an exception for an immutable sequence
self.assertRaises(TypeError, random.shuffle, (1,2,3))

def test_choice(self):
    element = random.choice(self.seq)
    self.assertTrue(element in self.seq)

def test_sample(self):
    with self.assertRaises(ValueError):
        random.sample(self.seq, 20)
    for element in random.sample(self.seq, 5):
        self.assertTrue(element in self.seq)

if __name__ == '__main__':
    unittest.main()
```


# Python 3 Upgrading

- https://caniusepython3.com/
- https://pypi.org/project/caniusepython3/
- https://docs.python.org/3/library/2to3.html
- http://python-future.org/futurize.html
- https://pypi.org/project/six/
- https://python-modernize.readthedocs.io/en/latest/
## 2to3

```bash
# Auto-correct files
2to3 -w whatever.py
```



# daemonizing

```python

 import sys, os
 def daemonize(stdin='/dev/null', stdout='/dev/null', stderr='/dev/null'):
 try:
 pid = os.fork()
 if pid > 0:
 sys.exit(0)
 except OSError, e:
 sys.stderr.write("fork #1 failed: (%d) %s\n" % (e.errno, e.strerror))
 
 os.chdir("/")
 os.umask(0)
 os.setsid()
 
 try:
 pid = os.fork()
 if pid > 0:
 sys.exit(0)
 except OSError, e:
 sys.stderr.write("fork #2 failed: (%d) %s\n" % (e.errno, e.strerror))
 sys.exit(1)
 
 for f in sys.stdout, sys.stderr: f.flush()
 si = file(stdin, 'r')
 so = file(stdout, 'a+')
 se = file(stderr, 'a+', 0)
 os.dup2(si.fileno(), sys.stdin.fileno())
 os.dup2(so.fileno(), sys.stdout.fileno())
 os.dup2(se.fileno(), sys.stdout.fileno())
```


# email

Run a simple SMTP server
------------------------



 python -m smtpd -n -c DebuggingServer localhost:1025

Send email
----------

```python

 import smtplib
 mail_server = 'localhost'
 mail_server_port = 25
 
 from_addr = 'sender@example.com'
 to_addr = 'receiver@example.com'
 
 from_header = 'From: %s\r\n' %from_addr
 to_header = 'To: %s\r\n\r\n' %to_addr
 subject_header = 'Subject: nothing interesting'
 
 body = 'This is a not-very-interesting email'
 
 email_message = '%s\n%s\n%s\n\n%s' % (from_header, to_header, subject_header, body)
 
 s = smtplib.SMTP(mail_server, mail_server_port)
 s.sendmail(from_addr, to_addr, email_message)
 s.quit()
```

## Retrieve mail with imaplib

<!-- TODO - ensure this works... -->

```python
import imaplib

username = 'name'
password = 'pass'

mail_server = 'mail_server'

i = imaplib.IMAP4_SSL(mail_server)
i.login(username, password)
i.select('INBOX')

for msg in i.search(None, 'ALL')[1][0].split():
    print msg
outf = open('%s.eml' % msg, 'w')
outf.write(i.fetch(msg, '(RFC822)')[1][0][1])
outf.close()
i.logout()
```



# ftplib

```python
from ftplib import FTP
server = FTP('ftp.example.com')
server.login()
```


# HTTPServer


Basic HTTP server
-----------------

```python

 from BaseHTTPServer import HTTPServer, BaseHTTPRequestHandler
 from SocketServer import ThreadingMixIn
 
 class RedirectHandler(BaseHTTPRequestHandler):
 def do_HEAD(self):
 self.send_response(200)
 self.send_header("Content-type", "text/html")
 self.end_headers()
 
 def do_GET(self):
 self.send_response(200)
 self.send_header("Content-type", "text/html")
 self.end_headers()
 self.wfile.write("<html><head><title>Example</title></head>")
 self.wfile.write("<body>%s</body></html>" % self.path)
 
 class ThreadedHTTPServer(ThreadingMixIn, HTTPServer):
 """
 Thread the responses
 """
 if __name__ == '__main__':
 host = "0.0.0.0"
 port = 8080
 try:
 httpd = ThreadedHTTPServer((host, port), RedirectHandler)
 httpd.serve_forever()
 except:
 print "Could not bind to host %s port %i\n" % (host, port)
```



## subprocess with Popen

```python
pipe = subprocess.Popen(args_list, shell=True, stdout=subprocess.PIPE)
output - pipe.communicate()[0]
```


## Installing virtualenv by source (Python 2)

```bash
wget <http://pypi.python.org/packages/source/v/virtualenv/virtualenv-1.8.4.tar.gz>
tar zxvf virtualenv-1.8.4.tar.gz
python setup.py install
```


# pdb
cmd | desc
--- | ---
h   | help
w   | (where) stacktrace
l   | list source
s   | step
n   | next
d   | down frame
b   | breakpoint
tb  | temporary breakpoint

## CLI

```bash
python -m pdb file.py
```

## Invoke debugger when line is hit

```python
# Python 3.7+
breakpoint()

# Python < 3.7
import pdb
pdb.set_trace()
```


# pydoc
```bash
# Read module docs in terminal
pydoc $MODULE

# Start documentation server, open browser
pydoc -b

# Start documentation server at port 1234
pydoc -p 1234
```
