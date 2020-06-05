@div
        beqi  $1, 0 -> error
loop:
	beq   $1, 1 -> end
	bodd  $1 -> slow
	sr    in0
	sr    in1
	b     loop
end:
	move  in0 -> quot
	store 0 -> rem
	return
slow:
	store 0 -> quot
slowloop:
	bgt   in1, rem -> slowend
	inc   quot
	sub   in0, in1 -> in0
	b     slowloop
slowend:
	move  in0 -> rem
	return
error:
	call  error, ("Division by zero")
