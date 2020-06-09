@insert
        beqi r1, 0 -> ins
        lw   r1 + 1 -> r2
        bgt  r2, r0 -> right
        bgt  r0, r2 -> left
        move r1 -> r0
        br   link
ins:
        addi sp, 2 -> sp
        sw   link -> sp - 2
        sw   r0 -> sp - 1
        li   3 -> r0
        addi ic, 1 -> link
        b    alloc
        lw   sp - 2 -> link
        lw   sp - 1 -> r1
        addi sp, -2 -> sp
        sw   r1 -> r0 + 1
        li   0 -> r1
        sw   r1 -> r0
        sw   r1 -> r0 + 2
        br   link
right:
        addi sp, 2 -> sp
        sw   link -> sp - 2
        sw   r1 -> sp - 1
        lw   r1 + 2 -> r1
        addi ic, 1 -> link
        b    insert
        lw   sp - 2 -> link
        lw   sp - 1 -> r1
        addi sp, -2 -> sp
        sw   r0 -> r1 + 2
        move r1 -> r0
        br   link
left:
        addi sp, 2 -> sp
        sw   link -> sp - 2
        sw   r1 -> sp - 1
        lw   r1 -> r1
        addi ic, 1 -> link
        b    insert
        lw   sp - 2 -> link
        lw   sp - 1 -> r1
        addi sp, -2 -> sp
        sw   r0 -> r1
        move r1 -> r0
        br   link
