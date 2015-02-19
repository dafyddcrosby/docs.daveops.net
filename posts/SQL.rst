SQL
---


To add a row
==============================
.. code-block:: sql

 INSERT INTO table_name (column1, column2, column3,...)
 VALUES (value1, value2, value3,...)

To update a row
==============================
.. code-block:: sql

 UPDATE table_name
 SET column1=value, column2=value2,...
 WHERE some_column=some_value

To sort a table
==============================
{{{
SELECT row1, row2
FROM table
ORDER BY row2 (ASC|DESC)
}}}
Delete rows
==============================
{{{
DELETE FROM table_name
WHERE some_column=some_value
}}}
Search in fields
==============================
{{{
SELECT *
FROM Persons
WHERE City LIKE '%ville'
}}}
Conditional statements
==============================
.. code-block:: sql

 CASE WHEN condition THEN result
 [WHEN ...]
 [ELSE result]
 END

Dates
==============================
Format a date
==============================
{{{
SELECT DATE_FORMAT(`date`,'%Y-%m-%d') AS showdate 
FROM table
}}}
Retrieve records within 90 days of stamp
==============================
{{{
SELECT expirationdate 
FROM stockserialitems
WHERE expirationdate < utc_timestamp() + interval 90 day
}}}
Check for duplicate rows
==============================
{{{
SELECT a, b, count(*) cnt 
FROM table
GROUP BY a, b 
HAVING cnt > 1
ORDER BY cnt asc;
}}}

