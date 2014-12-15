# Signals

A Clojure library for working with time-varying signals. 

## Highlights

* Pure Clojure, builds upon IRef/IDeref.
* Treats standard Clojure Atoms, Refs, Agents as time-varying, mutable signals. Users should use these in the normal way.
* Adds reactive events and signals that uses the watches feature of Clojure's IRef's. Reactive signals can also be watched by other reactive signals and events.
* Graph of signals and events can be dynamically modified. Reactors can be enabled and disabled.
* Builds upon the apply!\*! operator, which applies a function with dynamic args.  Used as the basis of lifting functions to work on signals.
* Signals can be lazy or strict: lazy signals do not update their values until they are dereferenced, while strict signals are updated immediately when dependents notify of a change.
* Seeks to make code that works with mutable values "highly visible" as well as isolated to be as small an area of the total codebase as possible. 
* Work with projections of data (CursorSignals) as a view upon a larger signal.  Allows reads from/writes to CursorSignals that operate with the original signal. 

## Usage

Please see the [documentation](doc/intro.md) for further information about the library.

Please see the [demo file](src/signals/demo.clj) for example usage.

## Influences

The following are projects that have influenced and continue to influence the development of Signals:

* [Elm](http://elm-lang.org)
* [Om](https://github.com/swannodette/om)
* [freactive](https://github.com/aaronc/freactive)
* [reagent](https://github.com/reagent-project/reagent)
* [reflex](https://github.com/lynaghk/reflex)
* [f-reactive](https://github.com/aaronc/freactive)
* [Zelkova](https://github.com/jamesmacaulay/zelkova)
* [Rx](https://github.com/ReactiveX/RxClojure)
* [Yampa](https://www.haskell.org/haskellwiki/Yampa)

## License

Copyright © 2014 Steven Yi 

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
