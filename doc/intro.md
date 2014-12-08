# Signals

This is a library for working with time-varying signals. It is currently in development and not yet ready for production use.  

## Definitions

* Signal - a time-varying source of data. Signals have their own time counter that is updated on each change of modification of state.  Time Counters are used to coordinate states for dependent signals. Signals may be lazy or strict: if strict, computation of signal is updated when dependencies change state; if lazy, computation of signal is done on first read after dependencies are updated. 
* MutableSignal - a time-varying source of data that is writable.
* Reactor - a block that reacts to signal changes. May or may not be a Signal in and of itself.   
* Timeline - an observer's view of signals over time.  A signal is an observer of its own timestate and thus has its own timeline. Timelines should be a total view of time over n number of signals. Multiple timelines can exist that share observations of signal times.  (Not yet implemented.)

## Design

Code should be classified primarily into:

* Input Code - where change is initiated.  May be due to things like keyboard input, hardware input, network input, etc. The code here will be mutators of MutableSignals. May have side-effects.     
* Application Code - Code that reacts to Input code.  Code here is mostly impure code, dealing with time-varying data. May have side-effects. 
* Library Code - Pure code. Should not have side effects. Will not be given Signals directly, only working with values. 

### Notes

* Signal Reactors should be referentially transparent in regards to time. For example, if a Reactor depends on 3 signals, and the signals have time counters 4, 5, and 6, then the Signal Reactor should yield the same value upon each dereferencing. 
* Each Signal must keep track of a time counter that is atomically updated upon each change of value.  
* History of values for a signal is not a primary concern of this library at this time, though it may be introduced later. 
