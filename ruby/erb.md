---
title: ERB
tags: ["ruby"]
---

<http://ruby-doc.org/stdlib-2.4.0/libdoc/erb/rdoc/ERB.html>

## Tags

```
<% Ruby code -- inline with output %>
<%= Ruby expression -- replace with result %>
<%# comment -- ignored -- useful in testing %>
% a line of Ruby code -- treated as <% line %> (optional -- see ERB.new)
%% replaced with % if first thing on a line and % processing is used
<%% or %%> -- replace with <% or %> respectively
```

## Trim mode

| %  |  enables Ruby code processing for lines beginning with % |
| <> |  omit newline for lines starting with <% and ending in %> |
| >  |  omit newline for lines ending in %> |
| -  |  omit blank lines ending in -%> |
