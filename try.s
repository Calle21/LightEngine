str:        "hej"

@main
        li   10 -> r0
fact:
        li   1 -> r1
loop:
        beqi r0, 1 -> end
        mul  r0, r1 -> r1
        b    loop
end:
        br   link
