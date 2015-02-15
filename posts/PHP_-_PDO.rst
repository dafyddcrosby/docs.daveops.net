PHP - PDO
---------
:tags: php


Simple query
==============================
{{{
$sql = "SELECT max(id) FROM table";
$sth = $DB->prepare($sql);
$sth->execute();
$max_id= $sth->fetchColumn();
}}}

