# validate-kv

[Key-Value形式](#Key-Value形式のファイルとは)のデータをバリデーションするコマンド

# Install

validate-kvはawkで実装されているため特別なインストールは不要です。適当なディレクトリに`validate-kv`ファイルを配置し、実行権限を付与してください。

# Requirement

- awk(gawk/nawk)
- POSIX Compatible Shell

# Usage

```txt
validate-kv - Key-Value形式のデータをバリデーションするコマンド

Usage:
    validate-kv <rule file> <data file>

Options:
    rule file: ルールファイル
    data file: データファイル

Exit Status:
    0: バリデーション成功
    1: バリデーションエラー

Output:
    標準出力: data fileの内容がそのまま出力されます
    標準エラー出力: バリデーションエラーが発生した場合のに<key> <rule>の形式で出力されます

Description:
    validate-kvは<rule file>に記載されたルールに従って
    <data file>のデータをバリデーションします。
    <data file>はKey-Value形式を前提としています。
```

# Example

使い方の例を示します。

ルールファイルを作成します。

```sh
$ cat rule
username    required    is_alnum    min_length 4
```

このルールファイルは以下のルールを定義しています。

- `username`は必須項目であり、半角英数字で構成され、4文字以上であること

次に、バリデーション対象のデータファイルを作成します。

```sh
$ cat data-valid
username fujis
```

最後に、`validate-kv`コマンドを実行します。

```sh
$ validate-kv rule data-valid
username fujis
$ echo $?
0
```

バリデーションが成功した場合、入力データがそのまま出力され、 終了ステータスは0になります。

次に、バリデーションエラーが発生するデータを作成します。

```sh
$ cat data-invalid
username fu#
```

`validate-kv`コマンドを実行します。

```sh
$ validate-kv rule data-invalid
username is_alnum
username min_length 4
$ echo $?
1
```

バリデーションエラーが発生した場合、違反したルールが出力され、終了ステータスは1になります。

# ルールファイルの書き方

## 構文

```
<Key>   <Validation Rule>
```

## Key

Keyは、データのフィールド名を指定します。英数字とアンダースコアのみを使用できます。

ex) `username`, `age`, `email`


## Validation Rule

| ルール                   | 説明                                                                 |
|--------------------------|----------------------------------------------------------------------|
| `alnum`                  | 値が英数字（アルファベットと数字）で構成されていることを確認します。 |
| `alpha`                  | 値がアルファベットで構成されていることを確認します。                 |
| `date`                   | 値が日付形式であることを確認します。                                 |
| `datetime`               | 値が日時形式であることを確認します。                                 |
| `datetim_tz`             | 値がタイムゾーン付き日時形式であることを確認します。                 |
| `digit`                  | 値が数字で構成されていることを確認します。                           |
| `email`                  | 値がメールアドレス形式であることを確認します。                       |
| `int`                    | 値が整数であることを確認します。                                     |
| `max_length <length>`    | 値の長さが指定された長さ以下であることを確認します。                 |
| `max_value <value>`      | 値が指定された値以下であることを確認します。                         |
| `min_length <length>`    | 値の長さが指定された長さ以上であることを確認します。                 |
| `min_value <value>`      | 値が指定された値以上であることを確認します。                         |
| `number`                 | 値が数値であることを確認します。                                     |
| `one_of[ <value> ... ]`  | 値が指定された値のいずれかであることを確認します。                   |
| `optional`               | 値が存在しなくてもよいことを示します。                               |
| `phone_number_jp`        | 値が日本の電話番号形式であることを確認します。                       |
| `regex <pattern>`        | 値が指定された正規表現パターンに一致することを確認します。           |
| `required`               | 値が必須であることを示します。                                       |
| `time`                   | 値が時刻形式であることを確認します。                                 |
| `zipcode_jp`             | 値が日本の郵便番号形式であることを確認します。                       |

## コメントとフォーマット

- `#` で始まる行はコメントとして扱われ、バリデーション処理時に無視されます。
- 空行は無視されます。
- トークンは1つ以上の連続したスペースで区切りられます。
- 人間の読みやすさのために適宜スペースを追加してください。

## 例

```
username         required    alnum    min_length 8    max_length 64
role             required    one_of[ employee area_manager system_admin ]
age              optional    int    min_value 0
gender           optional    one_of[ male female others ]
description      optional    text    min_length 1    max_length 128
zipcode          optional    zipcode_jp
address          optional    text    min_length 1    max_length 1024
email            optional    email
phone_number     optional    phone_number_jp
primary_contact  required    one_of[ email phone_number ]
```

このルールファイルは以下のルールを定義しています。

- `username`は必須項目であり、半角英数字で構成され、4文字以上64文字以下であること
- `role`は必須項目であり、`employee`, `area_manager`, `system_admin`のいずれかであること
- `age`は任意項目であり、0以上の整数であること
- `gender`は任意項目であり、`male`, `female`, `others`のいずれかであること
- `description`は任意項目であり、1文字以上128文字以下のテキストであること
- `zipcode`は任意項目であり、日本の郵便番号形式であること
- `address`は任意項目であり、1文字以上1024文字以下のテキストであること
- `email`は任意項目であり、有効なメールアドレス形式であること
- `phone_number`は任意項目であり、日本の電話番号形式であること
- `primary_contact`は必須項目であり、`email`, `phone_number`のいずれかであること


システム管理者からHTTP POSTなどで送信されたデータをkey-value形式で一時ファルに保存し、このルールを使ってバリデーションすることで、ユーザー登録情報の正当性を簡単に確認することができます。

## Key-Value形式のファイルとは

### 書式

```
第1フィールド 第2フィールド
```

### 説明

- 第1フィールドと第2フィールドが1行に1組ずつ記述されたキーと値のペアを記述したファイル。
- 第1フィールドと第2フィールドは1つ以上の連続した空白で区切られている。
- 第1フィールドにはキーを、第2フィールドには値を記述する。
- 第2フィールドには空白を含む文字列を記述することができる。

### 例

```
id 0001
first_name yamada
last_name taro
display_name taro yamada
```

この例では、以下のキーと値を持つデータを表しています。

| キー          | 値         |
|---------------|------------|
| id            | 0001       |
| first_name    | yamada     |
| last_name     | taro       |
| display_name  | taro yamada |

