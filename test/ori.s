@main
        li   20 -> r1
        ori  r1, 2 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
