 ; Prints a string
@puts (s)
    if s == 0 -> end
    putc s
    [s] = [s] + 1
    j   puts
end:
    return
