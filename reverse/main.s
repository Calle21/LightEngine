@main
   li   mem -> r0   ; prepare
   li   poi -> r1   ;   for
   sw   r0 -> r1    ;     alloc

   li   10 -> r0
   li   0 -> r1
   addi ic, 1 -> link
   b    make
   addi sp, 1 -> sp
   sw   r0 -> sp - 1
   addi ic, 1 -> link
   b    print
   lw   sp - 1 -> r0
   addi sp, -1 -> sp
   addi ic, 1 -> link
   b    reverse
   addi ic, 1 -> link
   b    print
   exit
