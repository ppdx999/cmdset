# argon2 password commands

## Installation

pwhash/psverify is a standalone python script file, so you can use it by copying it to a suitable location.

## Requirements

- Official supported Python versions
- argon2-cffi(`pip install argon2-cffi`)

## Algorithm

pwhash uses the Argon2id algorithm with the following parameters:

## Usage

```sh
$ pwhash <raw_password>
$ pwverify <hashed_password> <raw_password>
```

## License

Completely Public Domain Software (CC0)

This means everyone is free to use this software without any restrictions.
