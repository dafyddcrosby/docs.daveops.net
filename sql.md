# SQL
@databases

To add a row
------------

```sql
INSERT INTO table_name (column1, column2, column3,...)
VALUES (value1, value2, value3,...)
```

To update a row
---------------

```sql
UPDATE table_name
SET column1=value, column2=value2,...
WHERE some_column=some_value
```

To sort a table
---------------

```sql
SELECT row1, row2
FROM table
ORDER BY row2 (ASC|DESC)
```

Delete rows
-----------

```sql
DELETE FROM table_name
WHERE some_column=some_value
```

Search in fields
----------------

```sql
SELECT *
FROM Persons
WHERE City LIKE '%ville'
```

Conditional statements
----------------------

```sql
CASE WHEN condition THEN result
[WHEN ...]
[ELSE result]
END
```

Format a date
-------------

```sql
SELECT DATE_FORMAT(`date`,'%Y-%m-%d') AS showdate 
FROM table
```

Retrieve records within 90 days of stamp
----------------------------------------

```sql
FROM stockserialitems
WHERE expirationdate < utc_timestamp() + interval 90 day
```

Check for duplicate rows
------------------------

```sql
SELECT a, b, count(*) cnt 
FROM table
GROUP BY a, b 
HAVING cnt > 1
ORDER BY cnt asc;
```

Standards
---------

1992
1999
2003
2008
new ones?

