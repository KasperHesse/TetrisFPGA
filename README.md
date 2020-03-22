# TetrisFPGA
A version of the arcade classic Tetris, implemented on an FPGA

# What is this?
It's Tetris, on an FPGA. This means that *everything* that happens is entirely in hardware and not defined by software.

The entire project is written in [Chisel](https://github.com/freechipsproject/chisel3), a modern HDL. This project is essentially just a way of familiarising myself with the language

## How do I run it?
Get a working installation of Chisel/Scala by following [this handy guide](https://github.com/freechipsproject/chisel3/blob/master/SETUP.md). 
Then run
```
sbt "runMain Top"
```
To generate the corresponding Verilog code. Use Xilinx Vivado or your own favorite EDA Tool to put it onto an FPGA with a VGA port

##Requirements
To use this, you'll need an FPGA with a VGA port. This project is built for the [Basys3](https://store.digilentinc.com/basys-3-artix-7-fpga-trainer-board-recommended-for-introductory-users/) board from Digilent. 

Additionally, you must generate a 25.175MHz (or close enough) clock to drive the VGA timing signals. A 25MHz clock should work fine and is very easy to implement with two DFF's.

##Disclaimer
This project is *very much* a WIP. Nothing is finished, everything is up for change. Some comments may be missing, some stuff may be downright broken. But hey, learning by doing, right?