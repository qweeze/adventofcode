import re

f = open('day4.input')
passports = [{f.split(':')[0]: f.split(':')[1] for f in line.split()} for line in f.read().split('\n\n')]

def is_valid(p):
    requred_fields = ('byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid')
    return all(fld in p for fld in requred_fields)


def is_valid_strict(p):
    if not is_valid(p):
        return False
    if not (1920 <= int(p['byr']) <= 2002):
        return False
    if not (2010 <= int(p['iyr']) <= 2020):
        return False
    if not (2020 <= int(p['eyr']) <= 2030):
        return False

    if p['hgt'].endswith('cm'):
        if not 150 <= int(p['hgt'][:-2]) <= 193:
            return False
    elif p['hgt'].endswith('in'):
        if not 59 <= int(p['hgt'][:-2]) <= 76:
            return False
    else:
        return False

    if not re.match('^#[0-9a-f]{6}$', p['hcl']):
        return False
    if not p['ecl'] in 'amb blu brn gry grn hzl oth'.split():
        return False
    if not re.match('^[0-9]{9}$', p['pid']):
        return False

    return True

print(len(list(filter(is_valid, passports))))
print(len(list(filter(is_valid_strict, passports))))
