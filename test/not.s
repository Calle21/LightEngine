@main
        li   -2 -> r0
        not  r0 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
