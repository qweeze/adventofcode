from operator import mul
from functools import reduce

grid = [line.strip() for line in open('day3.input')]

def count_trees(slope_right, slope_down):
    n_trees = 0
    x, y = 0, 0
    while y < len(grid):
        if grid[y][x % len(grid[0])] == '#':
            n_trees += 1
        x += slope_right
        y += slope_down
    return n_trees

print(count_trees(3, 1))

print(reduce(mul, (
    count_trees(*slope)
    for slope in
    ((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
)))

