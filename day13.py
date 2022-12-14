#!/usr/bin/env python3
from typing import List, Any, Union
from functools import cmp_to_key

def compare(left, right) -> Union[bool,None]:
    """returns true if left is less than right"""
    if type(left) == int and type(right) == int:
        if left == right: return None
        else: return left <= right

    elif type(left) == list and type(right) == list:
        for l, r in zip(left, right):
            if type(l) == int and type(r) == int and l < r:
                return True
            else:
                res = compare(l, r)
                if res != None:
                    return res

        if len(left) < len(right):
            return True

        if len(left) > len(right):
            return False

    elif type(left) == int and type(right) == list:
        return compare([left], right)

    elif type(right) == int and type(left) == list:
        return compare(left, [right])

def solve(string: str) -> bool:
    """Returns true if l1 is greater than l2"""
    pairs = [list(map(eval, pair.split("\n"))) for pair in string[:-1].split("\n\n")]

    s = 0
    for idx, pair in enumerate(pairs):
        if compare(pair[0], pair[1]) == True:
            print(idx + 1)
            s += idx + 1

    return s

def bubble_sort(lists) -> List[Any]:
    """"""
def solve2(string: str) -> int:
    """Solves exercise nr. 2"""
    pairs = [list(map(eval, pair.split("\n"))) for pair in string[:-1].split("\n\n")] + [[[2]], [[6]]]

    sorted_pairs = sorted(pairs, key = cmp_to_key(compare))
    print(sorted_pairs)
    return sorted_pairs.index([[2]]) * sorted_pairs.index([[6]])

with open("data/day13_test.txt", "r") as file:
    contents = file.read()
    print(solve(contents))
    print(solve2(contents))
