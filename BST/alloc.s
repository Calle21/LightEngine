size, point, current, newc

@mem         |1024|
@poi         |1|

@alloc
        li   poi -> point
        lw   point -> current
        add  size, current -> newc
        sw   newc -> point
        move current -> r0
        br   link
