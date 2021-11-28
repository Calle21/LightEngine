@main
        li   8 -> r1
        addi r1, 10 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
