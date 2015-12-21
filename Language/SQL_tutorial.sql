-- display, order, filter
select column_1,column_2 | * from table_name;
select column_1,column_2 | * from table_name order by column_1 [desc];
select column_1,column_2 | * from table_name where column_1 operator some_value;

-- delete
delete * from table_name where some_column = some_value;
-- insert
insert into table_name values(value1, value2);
-- update
update table_name set column_1 = value1, column_2 = value2 where some_column = some_value;
-- get unique
select distinct column_name from table_name;
-- create new column
select *, expression as new_column_name from table_name;
