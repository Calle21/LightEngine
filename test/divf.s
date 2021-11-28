f0:         12.0
f1:         2.0

@main
        li   f0 -> r1
        lw   r1 -> r1
        li   f1 -> r2
        lw   r2 -> r2
        divf r1, r2 -> r0
        syscall pFloat
        li   10 -> r0
        syscall pChar
        exit
