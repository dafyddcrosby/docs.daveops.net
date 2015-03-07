Python - email
--------------
:tags: python 

Run a simple SMTP server
========================
::

 python -m smtpd -n -c DebuggingServer localhost:1025

Send email
==========
.. code-block:: python

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

Retrieve mail with imaplib
==============================
TODO - ensure this works...

.. code-block:: python

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
