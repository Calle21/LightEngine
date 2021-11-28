@main
        li   8 -> r1
        li   4 -> r2
        or   r1, r2 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
