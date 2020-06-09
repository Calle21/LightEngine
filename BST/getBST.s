str:        |100|
@get
        li   str -> r0
        syscall rString
        li   str -> r2
        li   0 -> r1
loop:
        lw   r2 -> r0
        beqi r0, 0 -> end
        addi sp, 2 -> sp
        sw   link -> sp - 2
        sw   r2 -> sp - 1
        addi ic, 1 -> link
        b    insert
        move r0 -> r1
        lw   sp - 2 -> link
        lw   sp - 1 -> r2
        addi sp, -2 -> sp
        addi r2, 1 -> r2
        b    loop
end:
        move r1 -> r0
        br   link
