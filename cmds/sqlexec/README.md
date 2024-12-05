# sqlexec

sqlread/sqlwrite is a simple command line tool to execute SQL queries on a database.

# Installation

sqlread/sqlwrite is standalone python script, so you can download the script and run it.

# Requirements

- Officially Supported Python
- sqlite3

# Usage

```sh
Usage:
    sqlread <sql-file> <parameter-file>
    sqlwrite <sql-file> <parameter-file>

sql-file:
    The file containing the SQL query to execute.

parameter-file:
    The file containing the parameters to be used in the SQL query.
    This file should be key-value format that the first space separate the key and value.
    If the sql query contains no parameters, this file can be omitted.

sqlread/sqlwrite uses the DATABASE_URL environment variable to connect to the database.
The DATABASE_URL environment variable should be in the format <engine>://<username>:<password>@<host>:<port>/<database>.

Engines and their respective URL formats are:

| Engine            | URL                                     | Is Supported?                           |
|-------------------|-----------------------------------------|-----------------------------------------|
| PostgreSQL        | postgres://USER:PASSWORD@HOST:PORT/NAME | No                                      |
| MySQL             | mysql://USER:PASSWORD@HOST:PORT/NAME    | No                                      |
| SQLite3           | sqlite3:///PATH                         | Yes                                     |
```

# Examples

```sh
$ export DATABASE_URL=sqlite:///test.db
sqlread test.sql
```
