current, prev, nxt
@reverse
    li   0 -> prev
loop:
    lw   current + 1 -> nxt
    sw   prev -> current + 1
    beqi nxt, 0 -> end
    move current -> prev
    move nxt -> current
    b    loop
end:
    br   link
