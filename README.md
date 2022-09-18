# ChillVm, a particularly not useful virtual machine written in Haskell 

ChillVm reads chill-bytecode, which is a sequnce of strings of '🤙' and '👍', delimited by '\n'.

Bytecode files contain bytecode instructions, one per line, encoded by 'chilling' and 'nice' emojis


ChillVm has 256 integer (8bit-)registers, can perform math operations on them

ChillVm can also perform ascii string operations (which is a big irony, considering the bytecode is utf),
but string size is limited to 256 chars, as well as string storage is same as integer registers

In other words, loading a 256-chars string will erase every register

*Checkout [a hello world example](https://github.com/ghB111/chill-vm/blob/main/ex-hello-world.txt) written in chill-bytecode*

# Build and run

```
git clone https://github.com/ghB111/chill-vm.git
cd chill-vm
./install-libs.sh
./build.sh
./build/chill-run ./ex-hello-world.txt
```

Chill vm has a set of particularly not useful instructions (0=🤙, 1=👍):

```
00000000: chl (chill~~, basically, nop)
00000001: ldc, 1: register idx, 2: constant to load
00000010: nul, 1: register to set to null
00000011: ld, 1: register idx, 2: register from which to copy
00000100: sgf, produce segfault (debug feature)
00000101: jmp, 1: where to jump
00000110: pls, 1: reg, 2: reg to store result (1+2)
00000111: mns, 1: reg, 2: reg to store result (1-2)
00001000: zbr, 1: where to jump, if last operation returned 0
00001001: bbr, 1: where to jump, if last operation returned >0
00001010: lbr, 1: where to jump, if last operation returned <0
00001011: cmp, same as mns, doesn't store result
00001100: prt, 1: register with pointer to where the string begins, 2: register where string length is stored
00001101: rdc, 1: register with pointer to place where a read char will be stored
11111111: stp, stops execution
```

Possible todos:
 - [x] Stop using unsafePerformIO
 - [x] Read user input
 - [ ] Macroinstructions
 - [ ] Assembly dsl?
 - [ ] Make it an actual stack project

