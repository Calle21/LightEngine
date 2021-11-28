@main
        li   11 -> r1
        li   -1 -> r2
        and  r1, r2 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
