@print
        beqi r0, 0 -> nothing
        addi sp, 2 -> sp
        sw   link -> sp - 2
        sw   r0 -> sp - 1
        lw   r0 -> r0
        addi ic, 1 -> link
        b    print
        lw   sp - 1 -> r1
        lw   r1 + 1 -> r0
        syscall pChar
        li   32 -> r0
        syscall pChar
        lw   r1 + 2 -> r0
        addi ic, 1 -> link
        b    print
        lw   sp - 2 -> link
        addi sp, -2 -> sp
nothing:
        br   link
