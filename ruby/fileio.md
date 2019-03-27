# FileIO
@Ruby

Write to a file
---------------

	fp = File.open(filename, mode)
	fp.write('bloop')
	fp.close

Read individual chars
---------------------

	fp = File.open(filename, mode)
	fp.each_char do |char|
	      puts char
	end
	fp.close

