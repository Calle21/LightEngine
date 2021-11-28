@main
        li   32 -> r1
        sri  r1, 1 -> r0
        addi r0, 8 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
