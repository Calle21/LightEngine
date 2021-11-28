@main
        li   -2 -> r1
        li   1 -> r2
        sra  r1, r2 -> r0
        addi r0, 16 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
