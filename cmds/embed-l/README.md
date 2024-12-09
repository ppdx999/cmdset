# embed

`embed-l` - embed data into a label-based template

## Usage

```sh
embed-l [<label>] <template-file> <data-file>
```

## Example

```terminal
$ cat data
a b c d

$ cat template
HEADER
___LABEL___
1st=%1
2nd=%2
3rd=%3 4th=%4
___LABEL___
FOOTER

$ embed-l ___LABEL___ template data
HEADER
1st=a
2nd=b
3rd=c 4th=d
FOOTER
```

```terminal
$ cat data
a b
c d

$ cat template
HEADER
___LIST___
1st=%1 2nd=%2
___LIST___
FOOTER

$ embed-l ___LIST___ template data
HEADER
1st=a 2nd=b
1st=c 2nd=d
FOOTER
```

If you omit the label, `___LABEL___` is used by default.

```terminal
$ cat data
a b c d
e f g h

$ cat template
HEADER
___LABEL___
1st=%1
2nd=%2
3rd=%3 4th=%4
___LABEL___

$ embed-l template data
HEADER
1st=a
2nd=b
3rd=c 4th=d
1st=e
2nd=f
3rd=g 4th=h
```

If `-u` or `--unescape` option is specified, the data is unescaped when embedded.

```terminal
$ cat data
1\t2_3 b
c d

$ cat template
HEADER
___LABEL___
1st=%1 2nd=%2
___LABEL___
FOOTER

$ embed-l -u template data
HEADER
1st=1	2 3 2nd=b
1st=c 2nd=d
FOOTER
```

Real-world example:

```terminal
$ cat data
text     username fujis
password password test1234

$ cat template
<form>
  <! ___LABEL___ >
  <input type="%1" name="%2" value="%3">
  <! ___LABEL___ >
  <button type="submit">Submit</button>
</form>

$ embed-l template data
<form>
  <input type="text" name="username" value="fujis">
  <input type="password" name="password" value="test1234">
  <button type="submit">Submit</button>
</form>
```
