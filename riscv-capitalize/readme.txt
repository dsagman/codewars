How to compile and run risk-v

assemble
riscv64-linux-gnu-as helloworld.s -o helloworld.o

link (may need to add -static)
riscv64-linux-gnu-ld helloworld.o -o helloworld

run (qemu-static)
qemu-riscv64 helloworld