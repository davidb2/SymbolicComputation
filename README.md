Symbolic Computation
====================

This library currently has the ability to parse a simple algebraic expression into meaningful code. In its current form, recognizable characters include:
  * `+`
  * `-`
  * `*`
  * `/`
  * `(`
  * `)`
  * `e`
  * `π`
  * `0-9`
  * all English letters (except `i`) as variables
  * all whitespace

Prerequisites
-------------
Make sure you install [`FSPowerPack`](https://www.nuget.org/packages/FSPowerPack.Community) and [`Unquote`](https://www.nuget.org/packages/Unquote) before trying to use this library. If you are using Visual Studio, run these commands in the Package Manage Console:
```
Install-Package FSPowerPack.Community
Install-Package Unquote
```
This will install the needed dependencies for you.


What's next?
------------
The library is missing a lot of useful features that will soon be implemented such as:
  * support for decimals
  * reduction of useless parentheses when printing
  * single variable derivation (maybe integration)
  * equations
  * ... and much more!

Credits
-------
Credit is given to [`lasandell`](https://github.com/lasandell/) and his [`FSharp Symbolic Math`](https://github.com/lasandell/FSharpSymbolicMath) repository for design choices made in this project.

Contribute
----------
Please feel free to contribute to this library and add issues if there is a problem!
