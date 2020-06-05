@mem        |1024|
@poi        |1|

@alloc
    la   poi -> $1
    lw   $1 -> $2
    add  $0, $2 -> $3
    sw   $3 -> $1
    move $2 -> $0
    br   $13
