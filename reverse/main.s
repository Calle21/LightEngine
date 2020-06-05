@main
   la   mem -> $0   ; prepare
   la   poi -> $1   ;   for
   sw   $0 -> $1    ;     alloc

   li   10 -> $0
   li   0  -> $1
   addi $15, 1 -> $13
   b    make
   sw   $0 -> $12
   addi $12, 1 -> $12
   addi $15, 1 -> $13
   b    print
   addi $12, -1 -> $12
   lw   $12 -> $0
   addi $15, 1 -> $13
   b    reverse
   addi $15, 1 -> $13
   b    print
   exit
