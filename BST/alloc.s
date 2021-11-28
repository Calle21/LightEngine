@mem         |1024|
@poi         |1|

@alloc
        li   poi -> r1
        lw   r1 -> r2
        add  r0, r2 -> r3
        sw   r3 -> r1
        move r2 -> r0
        br   link
