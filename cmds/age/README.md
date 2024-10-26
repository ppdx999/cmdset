# `age` - File Modification Age Checker

`age` is a versatile shell script that calculates the number of seconds that have passed since a specified file was last modified. Designed with flexibility in mind, it detects and uses available methods (`stat`, `ls --time-style`, or `find -printf`) to provide maximum compatibility across diverse UNIX-like environments.

## Usage

```sh
$ age <file>
```

### Arguments

- **`<file>`**: The absolute or relative path to the file for which you want to calculate the age (time since the last modification).

## Output

- If successful, `file_age.sh` outputs the number of seconds that have elapsed since the file was last modified.
- If the specified file cannot be found or accessed, or if no compatible method is available to retrieve the modification time, it displays an appropriate error message.

## Example

```sh
$ age file.txt
3600
```

In this example, the output `3600` indicates that 3,600 seconds (or 1 hour) have passed since the file was last modified.

## Implementation Details

The script tries multiple methods to retrieve the file's last modification time, in the following order of preference:

1. **GNU `stat`**: Retrieves the modification time directly in UNIX timestamp format if `stat` is available.
2. **`ls --time-style`**: Uses `ls` with `--time-style` option if supported to display the modification time in a readable format, then converts it to a timestamp.
3. **`find -printf`**: Uses `find -printf` as a last resort, formatting the modification time as a UNIX timestamp.

Each method is attempted sequentially, with the first successful result being used. If no method is compatible, the script will display an error message.

## Requirements

- UNIX-like operating system
- At least one of the following utilities: `stat`, `ls --time-style`, or `find -printf`

## Installation

1. Download `./sh/age` and place it in a directory included in your `$PATH`.
2. Make the script executable:

   ```sh
   chmod +x age
   ```

3. Run `age` from any directory.

## License

Complete Public-Domain Software (CC0)

It means that all of the people can use this for any purposes with no restrictions at all. By the way, We are fed up with the side effects which are brought about by the major licenses.
