# escape/unescape

`escape`/`unescape` is a command-line tool for escaping/unescaping IFS(whitespace, tab, newline, carriage return) in a string.

## Usage

```txt
$ escape <string>
$ unescape <string>
```

## Warning

This command assumes that the input string does not contain <\021> character, because it is used as a temporary stash character in the implementation.


## Effect

This command escapes/unescapes the following characters.

| Original Character | Escaped Character |
|--------------------|-------------------|
| `LF(0x0a)`         | `\n`              |
| `CR(0x0d)`         | `\r`              |
| `TAB(0x09)`        | `\t`              |
| `SPACE(0x20)`      | `_`               |
| `_`                | `\_`              |
