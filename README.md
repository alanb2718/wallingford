# Wallingford

Note: this project is currently inactive (the funding dried up ...)

Wallingford is an experimental constraint reactive programming
language written in Rosette.  It includes facilities for handling
persistent constraints, hard and soft constraints, and reactive
constraints.

Directories:
* core -- the core system
* applications -- various applications built on core
* reactive -- additional classes for adding reactive constraints to the core and applications 
* tests -- these use RackUnit

## Notes on installing Wallingford on Macintosh

Download Racket if need be.  Set up a symlink in /Applications from the
current version of Racket to Racket.  Add /Applications/Racket/bin to the
path in .profile

Follows the directions in the Rosette Guide for installing Rosette: https://docs.racket-lang.org/rosette-guide/ch_getting-started.html

Download Wallingford using "git clone https://github.com/alanborning/wallingford.git"

The hand-compiled version of the quadrilateral demo in
reactive/compiled-quadrilateral.rkt uses the Rhea implementation of the
Cassowary constraint solver.  To use this, check out the Racket bindings
for Rhea from https://github.com/cdglabs/rhea.git, do a make, and then
add a symlink from the wallingford directory to the Rhea directory.  The
symlink should be named just 'rhea' (this is already in .gitignore).

