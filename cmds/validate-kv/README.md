# validate-kv

A command for validating Key-Value formatted data.

TODO: Add a link to the page explaining the Key-Value format.

---

## Install

`validate-kv` is implemented in `awk`, so no special installation is required. Place the `validate-kv` file in an appropriate directory and grant it executable permissions.

---

## Requirements

- `awk` (gawk/nawk)
- POSIX Compatible Shell

---

## Usage

```txt
validate-kv - A command for validating Key-Value formatted data

Usage:
    validate-kv <rule file> <data file>

Options:
    rule file: The file containing validation rules.
    data file: The file containing data to be validated.

Exit Status:
    0: Validation succeeded.
    1: Validation failed.

Output:
    Standard Output: Outputs the contents of the data file as-is.
    Standard Error: Outputs validation errors in the format <key> <rule>.
    
Description:
    validate-kv validates the data in <data file> according to
    the rules defined in <rule file>.
    <data file> is expected to be in a Key-Value format.
```

---

## Example

### Basic Usage Example

Create a rule file.

```sh
$ cat rule
username    required    is_alnum    min_length 4
```

This rule file defines the following validation:

- `username` is required, must consist of alphanumeric characters, and must be at least 4 characters long.

Next, create a data file for validation.

```sh
$ cat data-valid
username fujis
```

Run the `validate-kv` command.

```sh
$ validate-kv rule data-valid
username fujis
$ echo $?
0
```

If validation succeeds, the input data is output as-is, and the exit status is `0`.

Now, create data that causes a validation error.

```sh
$ cat data-invalid
username fu#
```

Run the `validate-kv` command.

```sh
$ validate-kv rule data-invalid
username is_alnum
username min_length 4
$ echo $?
1
```

If a validation error occurs, the violated rules are output, and the exit status is `1`.

---

## Writing Rule Files

### Syntax

```
<Key>   <Validation Rule>
```

### Key

`Key` specifies the field name in the data. Only alphanumeric characters and underscores are allowed.

Examples: `username`, `age`, `email`

### Validation Rules

| Rule                   | Description                                                             |
|------------------------|-------------------------------------------------------------------------|
| `alnum`                | Ensures the value consists of alphanumeric characters.                 |
| `alpha`                | Ensures the value consists of alphabetic characters.                   |
| `date`                 | Ensures the value is in a valid date format.                           |
| `datetime`             | Ensures the value is in a valid datetime format.                       |
| `datetim_tz`           | Ensures the value is a datetime with a time zone.                      |
| `digit`                | Ensures the value consists of numeric digits.                          |
| `email`                | Ensures the value is in a valid email address format.                  |
| `int`                  | Ensures the value is an integer.                                       |
| `max_length <length>`  | Ensures the length of the value does not exceed the specified length.   |
| `max_value <value>`    | Ensures the value does not exceed the specified maximum value.          |
| `min_length <length>`  | Ensures the length of the value is at least the specified length.       |
| `min_value <value>`    | Ensures the value is at least the specified minimum value.              |
| `number`               | Ensures the value is numeric.                                          |
| `one_of[ <value> ... ]`| Ensures the value is one of the specified options.                      |
| `optional`             | Indicates the field is optional.                                       |
| `phone_number_jp`      | Ensures the value is in a valid Japanese phone number format.          |
| `regex <pattern>`      | Ensures the value matches the specified regex pattern.                 |
| `required`             | Indicates the field is required.                                       |
| `time`                 | Ensures the value is in a valid time format.                           |
| `zipcode_jp`           | Ensures the value is in a valid Japanese postal code format.           |

---

### Comments and Formatting

- Lines starting with `#` are treated as comments and ignored during validation.
- Blank lines are ignored.
- Tokens are separated by one or more spaces.
- Add spaces for readability where appropriate.

---

### Example

```
username         required    alnum    min_length 8    max_length 64
role             required    one_of[ employee area_manager system_admin ]
age              optional    int    min_value 0
gender           optional    one_of[ male female others ]
description      optional    text    min_length 1    max_length 128
zipcode          optional    zipcode_jp
address          optional    text    min_length 1    max_length 1024
email            optional    email
phone_number     optional    phone_number_jp
primary_contact  required    one_of[ email phone_number ]
```

This rule file defines:

- `username`: Required, alphanumeric, 8–64 characters.
- `role`: Required, must be `employee`, `area_manager`, or `system_admin`.
- `age`: Optional, integer, must be 0 or greater.
- `gender`: Optional, must be `male`, `female`, or `others`.
- `description`: Optional, text, 1–128 characters.
- `zipcode`: Optional, must match Japanese postal code format.
- `address`: Optional, text, 1–1024 characters.
- `email`: Optional, valid email format.
- `phone_number`: Optional, valid Japanese phone number format.
- `primary_contact`: Required, must be `email` or `phone_number`.

---

By saving Key-Value formatted data (e.g., user registration info) into a temporary file and validating it with these rules, you can easily ensure data correctness. For example, you can validate data submitted via HTTP POST requests using this system.
