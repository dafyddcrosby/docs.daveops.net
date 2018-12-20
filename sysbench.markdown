# sysbench
benchmark CPU
-------------


 sysbench --test=cpu --cpu-max-prime=20000 run

benchmark I/O
-------------
 

 sysbench --test=fileio --file-total-size=10G prepare
 sysbench --test=fileio --file-total-size=10G --file-test-mode=rndrw --init-rnd=on --max-time=300 --max_requests=0 run
 sysbench --test=fileio --file-total-size=10G cleanup



