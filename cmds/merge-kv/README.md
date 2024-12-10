# merge-kv

`merge-kv` - merge two key-value files.


# Usage

```sh
merge-kv <from-kv-file> <to-kv-file>
```

# Example 1

`from-kv-file` file:

```txt
username Mike
age 30
```

`to-kv-file` file:

```txt
username John
password test1234
```

Run:

```sh
$ merge-kv from-kv.txt to-kv.txt
username John
age 30
password test1234
```
