# paranet

Paranet is a simple runtime based on distributed computation.

It runs a little custom esoteric programming language describes lower in this document.


--------------------------

Paranet's programming language is very simple:

There are 4 main operators:

- `a | b` means that `a` and `b` are computed in parallel, and only the first
  result is kept.
- `a & b` means that `a` and `b` are computed sequentially (`b` will not get
  evaluated if `a` does not finish)
- `a ~ b` means that `a` and `b` are computed in parallel. Both results are kept
  (and ordered, that is to say that `a ~ b` will always return `resultof(a) &
  resultof(b)` and not in the oppposite order)
- `a -> b` means that `a` is computed and its result is passed as an argument of
  `b` (`b` resulting in a function)
  
For now, only integer computations and the first three operators above have been
implemented, yet I'm not sure more will be supported (as in functions and `->`)
at all.

---------------------------

The project consists of a main server charged of distributing (randomly)
computations to different clients.
I know that distributed cmputing does not necessarily work that way but I wanted
to keep it simple for me.

This is inspired by [lys](https://github.com/felko/lys) but is not meant to be
serious nor used at all. It was just a simple (but fun to develop) project.

----------------------------

What has been used:

- Haskell was my definite choice as a programming language for this language.
  It may seem strange as some things are overly complicated to do in Haskell, bu
  I find it very fun.
- I had to use the [STM](https://hackage.haskell.org/package/stm) (Software Transactional Memory) in order to perform
  atomic operations on global references not to break everything.
- First time using a GUI library ([qtah](https://github.com/Mesabloo/qtah) which
  I had to fork because of a small compatibility problems with the latest LTSs)
  and it really isn't easy to use at all.
- First time in networking too. Fortunately I had nothing to setup thanks to
  [network-run](https://hackage.haskell.org/package/network-run).
 
------------------------------

## Demo

![Video demo](./assets/demo.mp4)

------------------------------

This work is licensed under the [BSD3 license](./LICENSE). It is free to use by anyone.
