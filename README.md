# macrm

[![Build Status](https://travis-ci.org/satosystems/macrm.svg?branch=master)](https://travis-ci.org/satosystems/macrm)

This program is a replacement of macOS's `rm` command.
Unlike `rm` command, this `macrm` command moves deleted files to trash.
That is not just a move, of cause it is possible to undo.

## How to install

### Install via self build

```shell-session
$ git clone https://github.com/satosystems/macrm.git
...
$ cd macrm
...
$ make install
...
$
```

### Install via Homebrew

```shell-session
$ brew install satosystems/tap/macrm
...
$
```

### Install via Hackage

```shell-session
$ stack install macrm
...
$
```

## How to use

It is same as `rm` command.
Please see `macrm --help` for details.
