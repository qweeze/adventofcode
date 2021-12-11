seats = [line.strip() for line in open('day5.input')]
l = []
for seat in seats:
    lo, hi = 0, 127
    for x in seat[:7]:
        if x == 'F':
            hi = lo + (hi - lo) // 2
        else:
            assert x == 'B'
            lo = lo + (hi - lo) // 2
    row = hi

    lo, hi = 0, 7
    for x in seat[7:]:
        if x == 'L':
            hi = lo + (hi - lo) // 2
        else:
            assert x == 'R'
            lo = lo + (hi - lo) // 2
    col = hi
    l.append(row * 8 + col)

print(max(l))
l = set(l)
print([i for i in range(max(l)) if i not in l and i + 1 in l and i - 1 in l])
