# wallingford

Wallingford is an experimental constraint reactive programming
language written in Rosette.  It includes facilities for handling
persistent constraints, hard and soft constraints, and reactive
constraints.

Directories:
* core -- the core system
* applications -- various applications built on core
* reactive -- additional classes for adding reactive constraints to the core and applications 
* tests -- these use RackUnit

The hand-compiled version of the quadrilateral demo in
reactive/compiled-quadrilateral.rkt uses the Rhea implementation of the
Cassowary constraint solver.  To use this, check out the Racket bindings
for Rhea from https://github.com/cdglabs/rhea.git, do a make, and then
add a symlink from the wallingford directory to the Rhea directory.  The
symlink should be named just 'rhea' (this is already in .gitignore).
