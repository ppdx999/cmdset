# escape-html

`escape-html` is a command-line tool for escaping special characters used in HTML.

---

## Features

- **HTML Escaping**: Escapes special characters in the input string (e.g., `<`, `>`, `&`, `"`, `'`) to their corresponding HTML entities.
- **Simple Usage**: Reads data from standard input and outputs the escaped result to standard output.
- **Supports Piping**: Easily integrates with other commands using pipes.

---

## Usage

```sh
echo "<p>Hello & 'World' \"!\"</p>" | escape-html
&lt;p&gt;Hello &amp; &#39;World&#39; &quot;!&quot;&lt;/p&gt;
```

---

## Escaped Characters

| Original Character | Escaped Version |
|--------------------|-----------------|
| `<`                | `&lt;`         |
| `>`                | `&gt;`         |
| `&`                | `&amp;`        |
| `"`                | `&quot;`       |
| `'`                | `&#39;`        |

---

## Examples

### Escaping an Entire File

You can escape the entire content of a file and save it to a new file.

```sh
escape-html < input.html > escaped.html
```

### Combining with Other Commands

You can use it with commands like `cat` or `grep` in a pipeline.

```sh
cat input.html | grep "<p>" | escape-html
```

---

## Notes

- HTML escaping does not disable tags. If you need to prevent XSS or sanitize inputs, consider using appropriate libraries or frameworks.
- Be mindful of memory usage when processing very large inputs.

---

By using `escape-html`, you can ensure the safety of your HTML output and prevent unexpected rendering issues.
