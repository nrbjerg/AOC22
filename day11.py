#!/usr/bin/env python3
from dataclasses import dataclass
from typing import Callable, Tuple, List
import numpy as np
import sys
from copy import deepcopy

@dataclass()
class Monkey:
    """Models a single monkey."""
    worry_levels: List[int]
    operation: Callable[[int], int]
    divides: int
    throw_to: Tuple[int, int]
    times_inspected: int = 0

def simulate_rounds(monkeys: List[Monkey], n: int, question: int) -> List[Monkey]:
    """Simulates rounds of monkey buisness"""
    if n == 0:
        return monkeys

    divide_level = np.prod([monkey.divides for monkey in monkeys]) # NOTE: Tak mikkel :)
    for idx, monkey in enumerate(monkeys):
        monkeys[idx].times_inspected += len(monkey.worry_levels)

        for level in monkey.worry_levels:
            new_level = monkey.operation(level) % divide_level if question == 2 else monkey.operation(level) // 3
            monkeys[monkey.throw_to[int(not ((new_level % monkey.divides) == 0))]].worry_levels.append(new_level)

        monkeys[idx].worry_levels = []

    return simulate_rounds(monkeys, n - 1, question)

if __name__ == "__main__":
    monkeys = [
        Monkey([59, 74, 65, 86], lambda x: x * 19, 7, (6, 2)), Monkey([62, 84, 72, 91, 68, 78, 51], lambda x: x + 1, 2, (2, 0)), Monkey([78, 84, 96], lambda x: x + 8, 19, (6, 5)), Monkey([97, 86], lambda x: x * x, 3, (1,0)), Monkey([50], lambda x: x + 6, 13, (3, 1)), Monkey([73, 65, 69, 65, 51], lambda x: x * 17, 11, (4,7)), Monkey([69, 82, 97, 93, 82, 84, 58, 63], lambda x: x + 5, 5, (5,7)), Monkey([81, 78, 82, 76, 79, 80], lambda x: x + 3, 17, (3, 4))
    ]

    #
    times_inspected = [m.times_inspected for m in simulate_rounds(deepcopy(monkeys), 20, 1)]
    print(np.prod(sorted(times_inspected)[-2:]))
    # NOTE: Because deep copy because python objects a fucking muteable

    # 2.
    sys.setrecursionlimit(100_001)
    times_inspected = [m.times_inspected for m in simulate_rounds(monkeys, 10_000, 2)]
    print(np.prod(sorted(times_inspected)[-2:]))
