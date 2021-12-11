lines = open('input2.txt').readlines()
count =  0
for line in lines:
    policy, char, password = line.split()
    lo, hi = map(int, policy.split('-'))
    char = char[:-1]
    if lo <= password.count(char) <= hi:
        count += 1
print(count)

lines = open('input2.txt').readlines()
count =  0
for line in lines:
    policy, char, password = line.split()
    first, second = map(int, policy.split('-'))
    first -= 1
    second -= 1
    char = char[:-1]
    if not (password[first] == password[second] or (char not in (password[first], password[second]))):
        count += 1
print(count)
