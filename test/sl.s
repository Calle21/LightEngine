@main
        li   8 -> r1
        li   1 -> r2
        sl   r1, r2 -> r0
        addi r0, -3 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
