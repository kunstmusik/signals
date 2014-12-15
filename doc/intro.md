# Signals

This is a library for working with time-varying signals/mutable data. It works to leverage Clojure's existing IRef and IDeref interfaces for both push and pull based signal dependent code blocks.   

## Definitions

* Signal - a time-varying source of data. Signals may be lazy or strict: if strict, computation of signal is updated when dependencies change state; if lazy, computation of signal is done on first read after dependencies are updated. Can be watched. 
* MutableSignal - a time-varying source of data that is writable.
* PullSignal - a time-varying source of data that may change on each pull (deref). Does not participate in reactive graph. Is not a Signal or a Reactor.
* Reactor - a block that reacts to signal changes. May or may not be a Signal in and of itself.  
* SignalReactor - a block that is both a Signal and a Reactor.  It should be used as a Signal, as a source of data for other Reactors. It should not do side-effecty things. 

## Design

### Signals and Reactors 

Signals are sources of data, while Reactors react to changes in Signals. A signal may also be a Reactor. The Signals library is built upon Clojure's built-in IRef's.  Things like Atoms and Refs are recast as MutableSignals within the Signals library.  They can be watched, but also mutated.  All other dependent blocks are Reactors and Signals, but not MutableSignals.   

### Apply with Dynamic Arguments 

Reactors in Signals are based on apply!\*!, which takes a function and arguments and when called, will perform a map over arguments.  If an instance of IDeref is found, it is used as a signal and thus dereferenced.  If an arg is not an instance of IDeref, it is used as-is.  apply!\*! will then call apply with the mapped over arguments.  This change of normal function call processing captures the essence of dealing with mutable values by first making observations of all values, then passing just values to a function. The apply!\*! construct is the basis of Reactors and PullSignals. 

Note, apply!\*! is similar to Elm's lift, but arguments can be signals or non-signals and the result of apply!\*! is not a signal.  apply!\*! is a lower-level construct, which is used to build other Signals blocks, such as r!\*!, l!\*!, and e!\*!. 

### Lazy and Strict Signals

Signals allows expressing reactive code in lazy and strict ways.  Strict blocks react to other signals immediately, while lazy signals will track changes by dependencies and signal dependents, but they will not update their values until they are dereferenced. Lazy signals are useful for leaf signals of a signal graph, where they may not be reacted to directly but only sampled intermittently.  

### Usage 

Code should be classified primarily into:

* Input Code - where change is initiated.  May be due to things like keyboard input, hardware input, network input, etc. The code here will be mutators of MutableSignals. May have side-effects.     
* Application Code - Code that reacts to Input code.  Code here is mostly impure code, dealing with time-varying data. May have side-effects. 
* Library Code - Pure code. Should not have side effects. Will not be given Signals directly, only working with values. 

### Notes

* History of values for a signal is not a primary concern of this library at this time, though it may be introduced later. 
