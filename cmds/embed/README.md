# embed

`embed` - embed data into a template

## Usage

```sh
embed <template-file> <data-file>
```

## Example

```terminal
$ cat data
a b c d

$ cat template
1st=%1
2nd=%2
3rd=%3 4th=%4

$ embed template data
1st=a
2nd=b
3rd=c 4th=d
```

```terminal
$ cat data
a b
c d

$ cat template
1st=%1 2nd=%2

$ embed template data
1st=a 2nd=b
1st=c 2nd=d
```

```terminal
$ cat data
a b c
d e

$ cat template
1st=%1 2nd=%2
3rd=%3

$ embed template data
1st=a 2nd=b
3rd=c
1st=d 2nd=e
3rd=
```

```terminal
$ cat data
a b
d e f

$ cat template
1st=%1 2nd=%2 3rd=%3

$ embed template data
1st=a 2nd=b 3rd=
1st=d 2nd=e 3rd=f
```
