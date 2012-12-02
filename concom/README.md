# ConCom

A musical improvisation / composition program (to be) based on "concepts" (what ever that is).

For more information on the ideas behind it, see the project.org file (or its html version).

## Installation

ConCom comes with an ASDF system, so you can load it via Quicklisp or ASDF.
ConCom has been tested under SBCL on Linux, but it should run everywhere, where its dependencies run (bordeaux-threads and alexandria).

## Usage

So far the "brain" is running, but only for a meaningless implementation of "concepts" using integers. If you want to see it in action, run the following:

```lisp
> (defvar *brain* (concom:make-example-brain))
> (concom:start-thinking *brain*)
> (concom:stop-thinking *brain*)
```

`start-thinking` will start a thread and you will see a summary of the last thinking step every second. If you are using SLIME, you will have to either look at the inferior lisp buffer or add the following to `~/.swank.lisp`:
```lisp
(setf swank:*globally-redirect-io* t)
```
and then restart SLIME.

`stop-thinking` will stop the thinking-thread.

The output after "Thinking step:" shows:
* the concepts in the mind of the brain after the thinking step
* the number of concepts in the brain's memory after the step
* the concept used as input in this step
* the resulting new concept
