@main
        li   32 -> r1
        li   1 -> r2
        sr   r1, r2 -> r0
        addi r0, -2 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
