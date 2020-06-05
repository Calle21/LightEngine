@findChar
    lw   $1 -> $2
    beq  $0, $2 -> found
    beqi $2, 0 -> notfound
    addi $1, 1 -> $1
    b    findChar
found:
    move $1 -> $0
    br   $link
notfound:
    li   0 -> $0
    br   $link
