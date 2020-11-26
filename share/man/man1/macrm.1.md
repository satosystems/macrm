% MACRM(1) __VERSION__ | Command Manual
% Satoshi Ogata
% 2020-12-26

NAME
----

**macrm** -- remove directory entries to system trash

SYNOPSIS
--------

**macrm** [**-dfiPRrvW**] *file* *...*

DESCRIPTION
-----------

The **macrm** utility attempts to remove the non-directory type files specified on the command line. If the permissions of the file do not permit writing, and the standard input device is a terminal, the user is prompted (on the standard error output) for confirmation.

The options are as follows:

**-d** **--directory**

> Attempt to remove directories as well as other types of files.

**-f** **--force**

> Attempt to remove the files without prompting for confirmation, regardless of the file's permissions. If the file does not exist, do not display a diagnostic message or modify the exit status to reflect an error. The **-f** option overrides any previous **-i** options.

**-i** **--interactive**

> Request confirmation before attempting to remove each file, regardless of the file's permissions, or whether or not the standard input device is a terminal. The **-i** option overrides any previous **-f** options.

**-P** **--plaster**

> Overwrite regular files before deleting them. Files are overwritten three times, first with the byte pattern 0xff, then 0x00, and then 0xff again, before they are deleted. This flag is ignored under **macrm**.

**-R** **--recursive**

> Attempt to remove the file hierarchy rooted in each file argument. The **-R** option implies the **-d** option. If the **-i** option is specified, the user is prompted for confirmation before each directory's contents are processed (as well as before the attempt is made to remove the directory). If the user does not respond affirmatively, the file hierarchy rooted in that directory is skipped.

**-r**

> Equivalent to **-R**.

**-v** **--verbose**

> Be verbose when deleting files, showing them as they are removed.

**-W** **--whiteouts**

> Attempt to undelete the named files. Currently, this option can only be used to recover files covered by whiteouts. This flag is ignored under **macrm**.

**-?** **--help**

> Display help message

**-V** **--version**

> Print version information

**--numeric-version**

> Print just the version number

The **macrm** utility removes symbolic links, not the files referenced by the links.

It is an error to attempt to remove the files \`\`.'' or \`\`..''.

The **macrm** utility exits 0 if all of the named files or file hierarchies were removed, or if the **-f** option was specified and all of the existing files or file hierarchies were removed. If an error occurs, **macrm** exits with a value >0.

NOTE
----

The **macrm** command uses getopt(3) to parse its arguments, which allows it to accept the \`\--' option which will cause it to stop processing flag options at that point. This will allow the removal of file names that begin with a dash (\`-'). For example:

    macrm -- -filename

The same behavior can be obtained by using an absolute or relative path reference. For example:

    macrm /home/user/-filename
    macrm ./-filename

SEE ALSO
--------

rm(1)

BUGS
----

N/A

COMPATIBILITY
-------------

The **macrm** utility differs from historical implementations in that the **-f** option only masks attempts to remove non-existent files instead of masking a large variety of errors. The **-v** option is non-standard and its use in scripts is not recommended.

Also, historical BSD implementations prompted on the standard output, not the standard error output.

STANDARDS
---------

The **macrm** command is almost IEEE Std 1003.2 (\`\`POSIX.2'') compatible, except that POSIX requires **macrm** to act like rmdir(1) when the *file* specified is a directory. This implementation requires the **-d** option if such behavior is desired. This follows the historical behavior of **macrm** with respect to directories.

HISTORY
-------

A **macrm** command appeared in 2018-02-20.
