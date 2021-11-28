@main
        li   16 -> r1
        sli  r1, 1 -> r0
        addi r0, -9 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
