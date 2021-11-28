pl:         27

@main
        li   pl -> r1
        lw   r1 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        exit
