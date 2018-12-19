# UDP
Created Friday 13 October 2017

UDP fingerprinting
------------------
``In addition to his post, I heard a very good explanation of why they _intentionally_ coded it this way. The simplest way to create a packet is to simply set aside a section of memory for the packet and start filling in the necessary fields. For something like a UDP packet, there are some spaces in the packet that just aren't normally used. So if the field is left alne, is simply contains some data from whatever was stored in that section of memory previously. So instead of leaving those sections alone, they intentionally zeroed out the unused fields (such as the IP identification field) so that when the packet gets sent out, it doesn't give out any information that may have been laying around in memory.`` 
— from <http://www.antionline.com/showthread.php?221887.html>

