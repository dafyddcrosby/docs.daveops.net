===
RVM
===
:tags: ruby, ruby_on_rails


Install RVM with Ruby
==============================
::

 curl -L https://get.rvm.io | bash -s stable --ruby

Create a per-project rvmrc
==============================
::

 rvm --rvmrc --create 1.9.3@projectname

Gemsets
==============================
::

 rvm gemset [create|use|delete] gemsetname

See gem directory
==============================
::

 gem env gemdir

Installing openssl and readline for dependencies
================================================
::

 curl -L https://get.rvm.io | bash -s stable
 rvm pkg install openssl
 rvm pkg install readline
 rvm install 1.9.3 --with-openssl-dir=$HOME/.rvm/ --with-readline-dir=$HOME/.rvm/usr

Automatic gemset initialization
===============================
Add gems to ``~/.rvm/gemsets/global.gems``

