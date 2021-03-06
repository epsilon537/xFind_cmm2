xFind String Find across Files for CMM2 by Epsilon
--------------------------------------------------

Current Version: 0.2

Changelog
---------

0.2: Additional clarification in usage text.
0.1: Initial Version

Description
-----------

xFind finds all occurrences of a given text string across files specified by a filespec or directory.

Usage
-----

*xFind "text string" <filespec or dir>

Examples:

Search for text "SUB foo" in all .INC files in current directory: 
*xFind "SUB foo" *.INC

Search for text "foo$()" in directory "bar" (and subdirectories):
*xFind "foo$()" bar

If no filespec or directory is specified, all files in the current directory (and subdirectories) are searched.

Search is case insensitive.

Required CMM2 firmware version
------------------------------
V5.06.00

To Dos
------
Regular Expression matching

GitHub
------
https://github.com/epsilon537/xFind_cmm2
