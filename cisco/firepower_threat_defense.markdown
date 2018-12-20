# Firepower Threat Defense
Created Thursday 08 February 2018

Firepower Management Center
---------------------------

### Enabling the REST API

System>Configuration>REST API Preferences>Enable REST API

### CLI Modes

#### Regular Firepower Threat Defense CLI
The default
	>


#### Diagnostic
To enter, type ``system support diagnostic-cli``

user exec:
``firepower>``

privileged exec:
``firepower#``

#### Expert
To enter, type ``expert``

``admin@firepower:$``

### CA certs

	# get CAs for a trustpoint
	show crypto ca certificates [trustpointname]
	# show CA trustpoints
	show crypto ca trustpoints [trustpointname]

<https://www.cisco.com/c/en/us/td/docs/security/firepower/command_ref/b_Command_Reference_for_Firepower_Threat_Defense/s_3.html#wp1813115769>

