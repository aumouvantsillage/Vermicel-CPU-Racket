
Virgule - A minimal RISC-V core
===============================

Virgule is a 32-bit RISC processor core that implements most of the
base instruction set of the RISC-V specification (RV32I).
It was initially designed and implemented in VHDL for educational purposes,
but the VHDL source code is not publicly available.
You can read more about Virgule in [the documentation of its web-based simulator emulsiV](https://guillaume-savaton-eseo.github.io/emulsiV/doc).

The architecture of Virgule favours simplicity over completeness, speed or size.
If you are looking for an optimized, production-ready RISC-V core, there are
plenty of other implementations to choose from.

This repository contains an RTL model of Virgule written in Racket
using the techniques exposed in the blog post [Simulating digital circuits in Racket](http://guillaume.baierouge.fr/2021/03/14/simulating-digital-circuits-in-racket/index.html).
When executed, the Racket code performs a cycle-accurate simulation of the model.

This work is part of an ongoing experiment where Racket could serve
as a platform for the development of a future hardware description language.
There is currently no plan to generate Verilog or VHDL from Racket itself.

Content of this repository
--------------------------

The processor core is implemented in the following files in folder `src`:

* `opcodes.rkt`: constant definitions and basic instruction field decomposition.
* `datapath-components.rkt`: component descriptions for the datapath (ALU, comparator, registers, etc).
* `virgule.rkt`: the main processor description.

It makes use of these modules:

* `signal.rkt`: an implementation of hardware signals as data streams.
* `logic.rkt`: helpers to manipulate fixed-width binary data.

To integrate Virgule in a system, you can use:

* `memory.rkt`: memory components.
* `interconnect.rkt`: helpers to connect a processor with devices.

These modules can help create programs and analyze the behavior of a system:

* `assembler.rkt`: functions to generate machine code from assembly programs written as S-expressions.
* `vcd.rkt`: output signals to VCD (Value Change Dump) files.

The `examples` folder shows how a simple computer system can be described
and run with a small program:

```
racket examples/fibonacci.rkt
racket examples/hello.rkt
```

Dependencies
------------

This project depends on the following Racket libraries:

* collections-lib
* pvector
