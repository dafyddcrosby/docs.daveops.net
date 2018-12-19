# OpenACS
@Tcl

ad_proc documentation metadata
------------------------------
	@return
	@see
	@author
	@param
	@error


Return a file
-------------

	set file [open $file_path "r"]
	ns_set update [ns_conn outputheaders] content-disposition "attachment; filename=$filename"
	ns_returnfile 200 [ns_guesstype $file] $file_path


Make your OpenACS form’s checkbox be selected by default
--------------------------------------------------------
In ad_page_contract:

	{fries "t"}

In ad_form:

	{fries:text(checkbox),optional
	        {label {"Would you like fries with that?"}}
	        {html {[if {$fries=="t"} {return "checked checked"}]}}
	        {options {{"" t}}}
	}


ADP tags
--------
	<if @datasource.variable@ eq "blue">
	      <td bgcolor=#0000ff>
	</if>
	<elseif @datasource.variable@ eq "red">
	      <td bgcolor=red>
	</elseif>
	<else>
	      <td bgcolor=#ffffff>
	</else>

