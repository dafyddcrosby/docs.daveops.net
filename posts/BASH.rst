BASH
----
:date: 2015-04-05

Go immediately to $EDITOR
==============================
ctrl-x, e

Most commonly used commands
==============================
.. code-block:: bash

 history | awk '{a[$2]++}END{for(i in a){print a[i] " " i}}' | sort -rn | head

Delete files that are not extension
===================================
.. code-block:: bash

  rm !(*.foo|*.bar|*.baz)

Find, delete empty directories
==============================
.. code-block:: bash

 find . -type d -empty -exec rmdir {} \;

Show your shell from a port
==============================
.. code-block:: bash

 script -qf | tee >(nc -kl 5000) >(nc -kl 5001) >(nc -kl 5002)

Replace filename spaces with underscores
========================================
.. code-block:: bash

 for i in *; do mv "$i" "`echo $i| tr ' ' '_'`"; done

"Press any key to continue"
==============================
.. code-block:: bash

 read -sn 1 -p "Press any key to continue..."

Conditional Expressions
==============================

 +-----------+----------------------------------+
 | code      | desc                             |
 +===========+==================================+
 | -a file   | file exists                      |
 +-----------+----------------------------------+
 | -d file   | file exists and is a directory   |
 +-----------+----------------------------------+
 | -r file   | file exists and is readable      |
 +-----------+----------------------------------+
 | -w file   | file exists and is writeable     |
 +-----------+----------------------------------+
 | -x file   | file exists and is executable    |
 +-----------+----------------------------------+
 | -z string | true if length of string is zero |
 +-----------+----------------------------------+

Redirect STDERR to STDOUT
==============================
.. code-block:: bash

 command 2>&1

Process Substitution
====================
A temporary named pipe

.. code-block:: bash

 diff <(grep lines file1) <(grep lines file2)
 thing --output >(gzip > output.txt.gz)

Syntax cheatsheet
=================
.. code-block:: bash

 case expression in
    pattern1 )
        statements ;;
    pattern2 )
        statements ;;
    ...
 esac

 for VARIABLE in 1 2 3 4 5 .. N
 do
        command1
        command2
        commandN
 done
