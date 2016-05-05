This is a program that understands a shorthand for the kinds
of SQL that I usually write, specifically targeting SQLite.
It's hacky and bad and could use a lot of work, but it works
fine enough for me.

The input language is of the form

~~~
-- line comments
table1
  field1 : type1
  field2 : type2

table2
  field3 : type3
  field4 : type4
~~~

Fields can be indented any number of spaces. A new table is
started by an indentifier at the beginning of a line.

Types are either one of these built-in types:

~~~
null  int  integer  real  text  blob  date
~~~

or the name of a table declared in the source file.

All tables get an implicit `id` column of the form

~~~
  id INTEGER PRIMARY KEY ASC
~~~

and types which reference other tables are implicitly translated
into integers with foreign key constraints on the other table.
(This also means that it'll reject tables with an explicit
`id` column.)
For example, the following `electric-boogaloo` definition:

~~~
books
  title:       text
  author_name: authors
  published:   date

authors
  name:   text
  gender: blob
~~~

Produces the following SQLite table declarations:

~~~.sql
CREATE TABLE books
  ( id INTEGER PRIMARY KEY ASC
  , title TEXT
  , author_name INTEGER
  , published DATE
  , FOREIGN KEY(author_name) REFERENCES authors(id)
  );
CREATE TABLE authors
  ( id INTEGER PRIMARY KEY ASC
  , name TEXT
  , gender BLOB
  );
~~~
