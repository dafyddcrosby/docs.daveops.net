# Chef
@Chef

<https://www.inspec.io/>

Handle EC2 instance
-------------------

ec2 plugin installed with

	knife ec2 server create "role[ubuntu]" -I ami_id -f instance_type -S knife -i ~/.ssh/knife.pem --ssh-user ubuntu --region eu-west-1 -Z eu-west-1a

Install chef on RHEL 6 using gems
---------------------------------

Use the omnibus installer if you can!



 sudo rpm -Uvh <http://rbel.frameos.org/rbel6>
 yum install ruby ruby-devel ruby-ri ruby-rdoc ruby-shadow gcc gcc-c++ automake autoconf make curl dmidecode
 gem install chef --no-ri --no-rdoc

Using chef-solo
---------------


### /etc/chef/solo.rb

	json_attribs "/etc/chef/node.json"


### /etc/chef/node.json

	 {
	  "resolver": {
		"nameservers": [ "10.0.0.1" ],
		"search": "int.example.com"
	   },
	   "run_list": [ "recipe[resolver]" ]
	 }


knife search
------------



 knife search -a ATTR

common node attributes
----------------------

| description         | attribute                  |
|---------------------|----------------------------|
| version of chef     | chef_packages.chef.version |
| nodes's environment | chef.environment           |


Compile time notes
------------------


Use `lazy` so that the code block isn't evaluated until execution phase.

