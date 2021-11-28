@main
        li   1 -> r1
        li   2 -> r2
        add  r1, r2 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
