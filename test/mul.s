@main
        li   7 -> r1
        li   10 -> r2
        mul  r1, r2 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
