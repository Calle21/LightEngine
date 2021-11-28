@readint
    r2 = 0
loop:
    r1 = [r0]
    unless 48 <= r1 <= 58 -> end
    r0 = r0 + 1
    r1 = r1 - 48
    r2 = r2 * 10
    r2 = r2 + r1
    b    loop
end:
    r1 = r0
    r0 = r2
    return
    







 -- old
@readint
    li    0 -> $1
loop:
    lw    $0 -> $2
    addi  $0, 1 -> $0
    beqi  $2, 0 -> end
    addi  $2, -48 -> $2
    muli  $1, 10 -> $1
    add   $1, $2 -> $1
    b     loop
end:
    move  $0 -> $2
    move  $1 -> $0
    move  $2 -> $1
    br    $13
