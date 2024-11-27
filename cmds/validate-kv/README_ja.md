# validate-kv

Key-Value形式のデータをバリデーションするコマンド

todo: Key-Value形式の説明ページへのリンクを追加する

# Usage

```sh
validate-kv - Key-Value形式のデータをバリデーションするコマンド

Usage:
    validate-kv <rule file> <data file>

Options:
    rule file: ルールファイル
    data file: データファイル

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
:required username is_alnum    min_length 4
```

このルールファイルは以下のルールを定義しています。

- `username`は必須項目であり、半角英数字であること

次に、バリデーション対象のデータファイルを作成します。

```sh
$ cat data-valid
username fujis
```

最後に、`validate-kv`コマンドを実行します。

```sh
$ validate-kv rule data-valid
$ echo $?
0
```

バリデーションが成功した場合、コマンドは何も出力せず、終了ステータスは0になります。

次に、バリデーションエラーが発生するデータを作成します。

```sh
$ cat data-invalid
username fu#
```

`validate-kv`コマンドを実行します。

```sh
$ validate-kv rule data-invalid
username is_alnum min_length 4
$ echo $?
1
```

バリデーションエラーが発生した場合、違反したルールが出力され、終了ステータスは1になります。

# ルールファイルの書き方


ルールは大きく分けて以下の2つの要素から構成されます。

1. 項目検証 (Field Validation): 各データ項目の構造と制約を定義します。  
2. 制約検証 (Constraint Checking): 項目間の依存関係や論理的なルールを定義します。

---

## 1. 項目検証（Field Validation）

### 構文

```
:<存在式>   <Key>   <制約式>...
```

### 存在式

- `:required`: 必須項目であることを示します。
- `:optional`: 任意項目であることを示します。

### Key

Keyは、データのフィールド名を指定します。英数字とアンダースコアのみを使用できます。

ex) `username`, `age`, `email`


### 制約式

| ルール                    | 説明                                                                 |
|--------------------------|----------------------------------------------------------------------|
| `is_alnum`               | 値が英数字（アルファベットと数字）で構成されていることを保証します。        |
| `is_int`                 | 値が整数であることを保証します。                                        |
| `is_text`                | 値がテキストであることを保証します。                                    |
| `is_email`               | 値が有効なメールアドレス形式であることを保証します。                      |
| `is_zipcode_jp`          | 値が日本の郵便番号形式であることを保証します（例: `123-4567`）。           |
| `is_phone_number_jp`     | 値が日本の電話番号形式であることを保証します（例: `090-1234-5678`）。       |
| `is_one_of [値のリスト]`  | 値が指定されたリスト内のいずれかであることを保証します（例: `[ male female ]`）。 |
| `min_length <n>`         | テキストの長さが最低 `<n>` 文字以上であることを保証します。                |
| `max_length <n>`         | テキストの長さが最大 `<n>` 文字以下であることを保証します。                |
| `min_value <n>`          | 数値が最低 `<n>` 以上であることを保証します。                           |
| `max_value <n>`          | 数値が最大 `<n>` 以下であることを保証します。                           |

### 例

```
:required   username         is_alnum    min_length 4    max_length 64
:optional   age              is_int      min_value 0
:optional   gender           is_one_of [ male female others ]
:optional   description      is_text     min_length 1    max_length 128
```

## 2. 制約検証（Constraint Checking）

### 構文

```
<真偽式>      <論理式>
```

### 真偽式

- `:assert_true`: 指定した論理式が「真」である必要があることを示します。
- `:assert_false`: 指定した論理式が「偽」である必要があることを示します。


### 論理式

論理式は、項目の値や論理演算子を組み合わせて記述します。

EBNF形式で表すと以下のようになります：

```
<論理式> ::=
           | <not式>
           | <列挙値型前置単行演算式>
           | <項目値型後置単行演算式>
           | <論理式型中置二項演算式>
           | <is式>
           | '(' <論理式> ')'

<not式>                  ::= not <論理式>
<列挙値型前置単行演算式> ::= <列挙値型前置単項演算子> <列挙値>
<項目値型後置単行演算式> ::= <項目名> <項目値型後置単項演算子>
<論理式型中置二項演算式> ::= <論理式> <論理式型中置二項演算子> <論理式>
<is式>                   ::= <項目名> is <値>`

<列挙値型前置単項演算子> ::= exist_one_of | exist_two_of
<項目値型後置単項演算子> ::= is_exist | is_not_exist
<論理式型中置二項演算子> ::= or | and | == | implies | xor

<列挙値> ::= '[' <項目名> ( <項目名> )* ']'
<項目名> ::= 任意の文字列
<値>     ::= 任意の文字列
```

### 論理式の優先順位

1. 括弧内の式を最優先で評価。
2. not は最も強い優先順位を持つ。
3. 次に二項演算子の優先順位： and > or > implies > == > xor

### not式

BNF: `not <論理式>`

`not` は後続する論理式の否定を表します。

| 論理式 | not 論理式 |
|--------|------------|
| True   | False      |
| False  | True       |

### is式

BNF: `<項目名> is <値>`

データファイルの項目名の値が指定された値と等しい場合にTrueを返します。

### 列挙値型前置単項演算式

BNF: `<列挙値型前置単項演算子> <列挙値>`

| 列挙値型前置単項演算子 | 説明                                                                 |
|------------------------|--------------------------------------------------------------------|
| `exist_one_of`         | 列挙値のいずれかが存在する場合にTrueを返します。                        |
|  exist_two_of          | 列挙値の2つ以上が存在する場合にTrueを返します。                          |

### 項目値型後置単項演算式

BNF: `<項目名> <項目値型後置単項演算子>`


| 項目値型後置単項演算子 | 説明                                                                 |
|----------------|--------------------------------------------------------------------|
| `is_exist`     | 項目が存在する場合にTrueを返します。                                   |
| `is_not_exist` | 項目が存在しない場合にTrueを返します。                                  |

### 論理式型中置二項演算式

BNF: `<論理式> <論理式型中置二項演算子> <論理式>`


| 論理式型中置二項演算子 | 説明                                                                 |
|----------------|--------------------------------------------------------------------|
| `or`           | 論理和（例: `A or B` はAまたはBが真の場合に成立）。                  |
| `and`          | 論理積（例: `A and B` はAかつBが真の場合に成立）。                   |
| `==`           | 論理的同値（例: `A == B` はAとBが等しい場合に成立）。                |
| `implies`      | 論理的包含（例: `A implies B` はAが偽またはBが真の場合に成立）。       |
| `not`          | 否定（例: `not A` はAが偽の場合に成立）。                             |
| `xor`          | 排他的論理和（例: `A xor B` はAまたはBのどちらか一方が真の場合に成立）。 |

### 例

```
# メールアドレスまたは電話番号のいずれかが必須
:assert_true      email is_exist  or  phone_number is_exist

# 郵便番号と住所は両方存在するか、どちらも存在しない
:assert_true      zipcode is_exist  ==  address is_exist

# primary_contact が email の場合、email が存在する必要がある
:assert_true      primary_contact == email    implies     email is_exist
```

## 3. コメントとフォーマット

- `#` で始まる行はコメントとして扱われ、バリデーション処理時に無視されます。
- 空行は無視されます。
- トークンは1つ以上の連続したスペースで区切り、読みやすさのために適宜スペースを追加してください。

### 例

```
# 必須項目
:required   username         is_alnum    min_length 4    max_length 64
:required   primary_contact  is_one_of [ email phone_number ]

# 任意項目
:optional   age              is_int      min_value 0
:optional   email            is_email
:optional   phone_number     is_phone_number_jp

# 条件
:assert_true      require_one_of [ email phone_number ]
:assert_true      zipcode is_exist  ==  address is_exist
```

# Realworld Example

実業務での利用例として、システム管理者がユーザーを新規に追加するときのユーザー登録情報のバリデーションルールを考えます。

```
$ cat rule
# 項目
:required   username         is_alnum    min_length 4    max_length 64
:required   role             is_one_of [ employee area_manager system_admin ]
:optional   age              is_int    min_value 0
:optional   gender           is_one_of [ male female others ]
:optional   description      is_text    min_length 1    max_length 128
:optional   zipcode          is_zipcode_jp
:optional   address          is_text    min_length 1    max_length 1024
:optional   email            is_email
:optional   phone_number     is_phone_number_jp
:required   primary_contact  is_one_of [ email phone_number ]

# 条件
:assert_true      require_one_of [ email phone_number ]
:assert_true      zipcode is_exist  ==  address is_exist
:assert_true      primary_contact == email    implies     email is_exist
:assert_true      primary_contact == phone_number    implies    phone_number is_exist
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
- `email`または`phone_number`のいずれかが必須であること
- `zipcode`と`address`は両方存在するか、どちらも存在しないこと
- `primary_contact`が`email`の場合、`email`が存在すること
- `primary_contact`が`phone_number`の場合、`phone_number`が存在すること


システム管理者からHTTP POSTなどで送信されたデータをkey-value形式で一時ファルに保存し、このルールを使ってバリデーションすることで、ユーザー登録情報の正当性を簡単に確認することができます。
