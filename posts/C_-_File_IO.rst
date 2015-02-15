C - File IO
===========
:tags: c


Open and read file
------------------
.. code-block:: c

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
