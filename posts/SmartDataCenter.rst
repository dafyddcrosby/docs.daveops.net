SmartDataCenter
---------------


Find VM by alias
==============================
{{{
sdc-vmapi /vms | json -H -c "this.alias &amp;&amp; this.alias.match(/riak/)"
}}}
Output fwapi rules
==============================
{{{
sdc-fwapi /rules
}}}

