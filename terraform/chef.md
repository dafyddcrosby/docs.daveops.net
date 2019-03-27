# chef
Created Sunday 24 September 2017

	
	provisioner "chef" {
	
	  # add instance to chef-vault
	  vault_json = <<EOF
	{
	  "databag": [
	    "item1",
	    "item2"
	  ]
	}
	EOF
	}
	

