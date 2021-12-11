from itertools import combinations
from functools import reduce
from operator import mul

numbers = list(map(int, open('input.txt').readlines()))
result = reduce(mul, next(filter(lambda triple: sum(triple) == 2020, combinations(numbers, 3))))
print(result)

