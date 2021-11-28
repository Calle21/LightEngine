@main
        li   40 -> r1
        divi r1, 2 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
