# embed

`embed` is a simple command-line tool that embed data into a template file using a keyword

## Usage

```sh
embed <keyword> <template-file> <data-file>
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

Run the `embed` command to insert the data into the template:

```sh
$ embed ___MAIN_CONTENT___ template.html content.html
<body>
<h1>Title</h1>
<main>
<p>Content</p>
</main>
</body>
```


## Features

- **Simple Replacement**: Replaces the specified keyword with the exact contents of the data file.
- **Reusability**: Allows easy application of the same layout with different data by using template files.
- **Standard Output**: Outputs the result to standard output, making it easy to redirect or pipe the output for further processing.


## Notes

- `<keyword>` should be unique within the template file to avoid unexpected replacements.
- The contents of the data file are inserted as-is, without any special escaping. To avoid unintended results, verify the data file's contents beforehand.
