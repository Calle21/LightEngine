@sumArrays (len, arr) ([acc], offset)
    acc, offset = 0
loop:
    if offset == len -> end
    acc = acc + sumArray (len, [arr + offset])
    offset = offset + 1
end:
    return

@sumArray (len, arr) ([acc], offset)
    acc, offset = 0
loop:
    if offset == len -> end
    acc = acc + [arr + offset]
    offset = offset + 1
    b    loop
end:
    return
