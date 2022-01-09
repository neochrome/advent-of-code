import collections
import re


def checksum(text):
    freqs = collections.Counter(text)
    if '-' in freqs:
        del freqs['-']
    return str.join('', map(lambda x: x[0], sorted(freqs.items(), key=lambda x: (-x[1], x[0]))[:5]))


def rofl(l, n):
    if l == '-':
        return ' '
    return chr(97 + (ord(l) - 97 + n) % 26)


with open("./input") as f:
    idsum = 0
    for line in f:
        m = re.search('([a-z-]+)-(\d+)\[(\w+)\]', line.rstrip('\n'))
        if checksum(m.group(1)) != m.group(3):
            continue
        id = int(m.group(2))
        idsum += id
        decoded = str.join('', [rofl(c, id) for c in m.group(1)])
        if decoded == 'northpole object storage':
            print('part2: ', id)
    print('part1: ', idsum)
