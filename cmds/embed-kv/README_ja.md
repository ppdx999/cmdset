# embed-kv

`embed-kv` は、テンプレートファイルにキーと値のペアを挿入するためのコマンドラインツールです。

# 使用方法

```sh
embed-kv <template-file> <kv-file>
```

- `<template-file>`: テンプレートファイルへのパス。
- `<kv-file>`: 挿入するキーと値のペアを含むファイルへのパス。

# 例 1

`template` ファイル:

```txt
Hello ___USER_NAME___!
I hope you have a great day.

Best regards,
___SENDER_NAME___
Date: ___DATE___
```

`kv` ファイル:

```txt
USER_NAME John
SENDER_NAME Jane
DATE 2021-01-01
```

`embed-kv` コマンドを実行してテンプレートにキーと値のペアを挿入します:

```sh
$ embed-kv template.txt kv.txt
Hello John!
I hope you have a great day.

Best regards,
Jane
Date: 2021-01-01
```

---

# 例 2

テンプレートファイルを作成:

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

キーと値のペアファイルを作成:

```sh
$ cat <<EOF > error_messages.kv
___error_username___ <p class="error">at least 3 characters long</p>
___error_password___ <p class="error">only alphanumeric characters allowed</p>
EOF
```

`embed-kv` コマンドを実行してテンプレートにキーと値のペアを挿入します:

```sh
$ embed-kv template.html error_messages.kv
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
