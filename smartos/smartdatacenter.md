# SmartDataCenter
@SmartOS

Find VM by alias
----------------



 sdc-vmapi /vms | json -H -c "this.alias &amp;&amp; this.alias.match(/riak/)"

Output fwapi rules
------------------



 sdc-fwapi /rules

Update resolvers
----------------



 sdc-vmapi  /vms/<uuid>?action=update -d '{ "resolvers": ["8.8.8.8", "8.8.4.4"] }'

