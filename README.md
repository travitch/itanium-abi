This package implements the Itanium ABI standard for C++ name mangling
and demangling.

Despite the name, this standard covers name mangling for g++ on all
platforms that do not define their own ABI (including x86 and x86_64).
The package includes a name demangler and re-mangler.  It also
includes a pretty printer to render C++ types as pseudo-declarations
for easy inspection, as well as more structured traversals over an
ADT.

The test suite is reasonable and compares the output of this package
against that of the _demangle_ tool included with binutils.  It looks
like everything that should pass does.

The implementation conforms to the specification detailed at
[codesourcery](http://sourcery.mentor.com/public/cxx-abi/abi.html).
It is built using
[boomerang](http://hackage.haskell.org/package/boomerang) to construct
the parser and unparser from a single specification.

## TODO

Most missing features are related to either templates or C++11.  These
will be implemented eventually.

 * Support for template names
 * Support for decltype
 * Support for lambdas
 * Support for scope disambiguators
