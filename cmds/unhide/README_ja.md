# unhide
指定した隠しファイルを簡単に表示させるための小さなコマンドです。

# インストール

```terminal
$ cd unhide
$ ./install.sh -d /usr/local/bin
```

# 使用方法

```
unhide <ファイル> ...
```

# 使用例

```terminal
$ touch .a_file

$ ls
.a_file

$ unhide .a_file

$ ls
a_file
```

# ライセンス

完全なパブリックドメインソフトウェア（CC0）

これは、誰でも、いかなる制約もなくあらゆる目的でこのソフトウェアを使用できることを意味します。ところで、私たちはメジャーなライセンスによる副作用にうんざりしています。
