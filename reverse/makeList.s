@make
    addi sp, 1 -> sp
    sw   link -> sp - 1
    addi ic, 1 -> link
    b    cons
    lw   sp - 1 -> link
    addi sp, -1 -> sp
    move r0 -> r1
    lw   r1 -> r0
    beqi r0, 0 -> end
    addi r0, -1 -> r0
    b    make
end:
    move r1 -> r0
    br   link
