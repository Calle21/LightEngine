fl:         3.2

@main
        li   fl -> r0
        lw   r0 -> r0
        syscall pFloat
        li   10 -> r0
        syscall pChar
        exit
