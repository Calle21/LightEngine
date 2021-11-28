@main
        li   2 -> r1
        move r1 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
