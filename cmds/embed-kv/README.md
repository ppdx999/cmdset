# embed-kv

`embed-kv` is a command-line tool that embeds key-value pairs into a template file.

# Usage

```sh
embed-kv <template-file> <kv-file>
```

- `<template-file>`: Path to the template file.
- `<data-file>`: Path to the file containing key-value pairs to be inserted into the template.

# Example 1

`template` file:

```txt
Hello ___USER_NAME___!
I hope you have a great day.

Best regards,
___SENDER_NAME___
Date: ___DATE___
```

`kv` file:

```txt
USER_NAME John
SENDER_NAME Jane
DATE 2021-01-01
```

Run the `embed-kv` command to insert the key-value pairs into the template:

```sh
$ embed-kv template.txt kv.txt
Hello John!
I hope you have a great day.

Best regards,
Jane
Date: 2021-01-01
```

# Example 2

Create a template file:

```sh
$ cat <<EOF > template.html
<body>
<h1>Title</h1>
<form>
<label for="username">Username:</label>
<input type="text" id="username" name="username">
___error_username___
<label for="password">Password:</label>
<input type="password" id="password" name="password">
___error_password___
<button type="submit">Submit</button>
</form>
</body>
EOF
```

Create a key-value file:

```sh
$ cat <<EOF > content.kv
___error_username___ <p class="error">at least 3 characters long</p>
___error_password___ <p class="error">only alphanumeric characters allowed</p>
EOF
```

Run the `embed-kv` command to insert the key-value pairs into the template:

```sh
$ embed-kv template.html content.kv
<body>
<h1>Title</h1>
<form>
<label for="username">Username:</label>
<input type="text" id="username" name="username">
<p class="error">at least 3 characters long</p>
<label for="password">Password:</label>
<input type="password" id="password" name="password">
<p class="error">only alphanumeric characters allowed</p>
<button type="submit">Submit</button>
</form>
</body>
```
