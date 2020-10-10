import string
import random

def atom_generator(size=10, chars=string.ascii_lowercase):
    return ''.join(random.choice(chars) for _ in range(size))

def create_file(size=1000000):
    intervals = []
    for i in range(size):
        name = atom_generator()
        start = random.randint(0, 500000)
        intervals.append([name, 'start', start])
        intervals.append([name, 'end', random.randint(start, 500000)])
    intervals.sort(key=lambda x: x[2])
    with open('intervals.pl', 'w') as file:
        for interval in intervals:
            file.write(f'{interval[1]}({interval[0]}, {interval[2]}).\n')

create_file()
