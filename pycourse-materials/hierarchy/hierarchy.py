import sys
from collections import defaultdict


def main():
    num_lines = int(raw_input())
    manages = defaultdict(list)
    ceo = None
    for _ in xrange(num_lines):
        line = raw_input()

        emp_a, emp_b = line.split(" ", 1)
        manages[emp_a].append(emp_b)

        if ceo is None:
            ceo = emp_a

    level = [ceo]
    next_level = list()

    while level:
        emp = level.pop()
        print emp,
        next_level.extend(manages[emp])

        if not level:
            print
            level, next_level = next_level, level


if "__main__" == __name__:
    main()
