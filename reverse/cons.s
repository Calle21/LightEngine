@cons
    addi sp, 3 -> sp
    sw   r0 -> sp - 3
    sw   r1 -> sp - 2
    sw   link -> sp - 1
    li   2 -> r0
    addi ic, 1 -> link
    b    alloc
    lw   sp - 1 -> link
    lw   sp - 2 -> r2
    lw   sp - 3 -> r1
    addi sp, -3 -> sp
    sw   r1 -> r0
    sw   r2 -> r0 + 1
    br   link

