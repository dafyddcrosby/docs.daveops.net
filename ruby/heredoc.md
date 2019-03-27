# heredoc
Created Wednesday 20 September 2017

	message1 = <<EOM
	This string starts at line start
	EOM # Needs to be at 0 position
	
	# TODO <<-EOM
	
	message3 = <<~EOM
	  This one will remove space before first printable character
	  Available in Ruby 2.3+
	EOM


