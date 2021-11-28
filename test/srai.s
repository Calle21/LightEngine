@main
        li   -1 -> r1
        srai r1, 1 -> r0
        addi r0, 26 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
