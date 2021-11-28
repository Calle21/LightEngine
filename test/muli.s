@main
        li   19 -> r1
        muli r1, 10 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
