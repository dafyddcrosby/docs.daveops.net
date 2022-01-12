# Net::SMTP

<https://ruby-doc.org/stdlib-2.3.0/libdoc/net/smtp/rdoc/Net/SMTP.html>

Using starttls
--------------
	smtp = Net::SMTP.new smtp_options[:address], smtp_options[:port]
	smtp.enable_starttls_auto
	smtp.start(
	  smtp_options[:helo_domain],
	  smtp_options[:user_name],
	  smtp_options[:password],
	  smtp_options[:authentication]
	) do |smtp|
	  smtp.send_message msgstr, "from@example.org", [ "to@example.org" ]
	end

