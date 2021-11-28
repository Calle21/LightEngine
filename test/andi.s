@main
        li   -1 -> r1
        andi r1, 21 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
