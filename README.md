# Signals

A Clojure library for working with time-varying signals. 

Features
* Builds on top of Clojure's Atoms, Refs, Agents, treating them as time-varying, mutable signals. Users should use these in the normal way.
* Adds reactive events and signals that uses the watches feature of Clojure's IRef's. Reactive signals can also be watched by other reactive signals and events.
* Builds upon the apply!\*! operator to apply a function with dynamic args.  
* Signals can be lazy or strict: lazy signals do not update their values until they are dereferenced, while strict signals are updated immediately when dependents notify of a change.
* Seeks to make code that works with mutable values "highly visible" as well as isolated to be as small an area of the total codebase as possible. 

## Usage

Please see the [documentation](doc/intro.md) for further information about the library.

## License

Copyright © 2014 Steven Yi 

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
