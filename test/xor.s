@main
        li   10 -> r1
        li   10 -> r2
        xor  r1, r2 -> r0
        addi r0, 16 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
