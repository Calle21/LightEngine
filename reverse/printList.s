@print
    move $0 -> $1
    lw   $1 -> $0
    syscall 2
    addi $1, 1 -> $1
    lw   $1 -> $0
    beqi $0, 0 -> end
    b    print
end:
    br   $13
