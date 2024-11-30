# embed-f

`embed-f` is a simple command-line tool that embed data file into a template file using a keyword

## Usage

```sh
embed-f <keyword> <template-file> <data-file>
```

- `<keyword>`: The string in the template file to be replaced (e.g., `___MAIN_CONTENT___`).
- `<template-file>`: Path to the template file.
- `<data-file>`: Path to the file containing data to be inserted into the template.


## Example

Create a template file:

```sh
$ cat <<EOF > template.html
<body>
<h1>Title</h1>
<main>
<!-- ___MAIN_CONTENT___ -->
</main>
</body>
EOF
```

Create a data file:

```sh
$ cat <<EOF > content.html
<p>Content</p>
EOF
```

Run the `embed-f` command to insert the data into the template:

```sh
$ embed-f ___MAIN_CONTENT___ template.html content.html
<body>
<h1>Title</h1>
<main>
<p>Content</p>
</main>
</body>
```
