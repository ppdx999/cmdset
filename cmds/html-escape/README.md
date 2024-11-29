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
