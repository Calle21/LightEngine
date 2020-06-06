@print
    move r0 -> r1
loop:
    lw   r1 -> r0
    syscall pInt
    li   32 -> r0
    syscall pChar
    lw   r1 + 1 -> r1
    beqi r1, 0 -> end
    b    loop
end:
    br   link
