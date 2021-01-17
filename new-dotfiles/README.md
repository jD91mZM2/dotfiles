# My new NixOS configuration

This is a WORK IN PROGRESS new configuration for my systems. The goal is to get
rid of the complexity my abstractions had previously, and make each module just
a little bit more *pure*. The problem with my previous dotfiles was that they
were too complex. I tried too hard to abstract over everything, and the
abstractions didn't work by themselves.

The idea here is simple. Each system will consist of a series of imports,
imports of different modules that do one thing in a very opinionated way. No
configuration, no fancy options, just a simple thing. The only configuration
needed is whether or not the module is imported or not. Obviously each module in
`modules/` must either be standalone or import all required modules.
