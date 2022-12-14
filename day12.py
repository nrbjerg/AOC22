#!/usr/bin/env python3
from __future__ import annotations
import numpy as np
from itertools import product
import os
from dataclasses import dataclass
from typing import List, Tuple
import sys

@dataclass()
class Node:
    """A node in the graph"""
    neighbours: List[Node|None]
    pos: Tuple[int, int]
    visited: bool = False
    distance: float = np.inf

    def __eq__ (self, n: Node) -> bool:
        """Returns true if the two elements are equivilent"""
        return n.pos == self.pos



class Graph:

    def __init__ (self, lines: List[str], start: Tuple[int, int], end: Tuple[int, int]):
        """ initializes the graph. """
        self.height_map = [[c for c in line] for line in lines]
        self.dimensions = (len(self.height_map), len(self.height_map[0]))
        self.nodes = self.create_and_connect_nodes(self.is_passable)
        self.start = self.nodes[start[0]][start[1]]
        self.end = self.nodes[end[0]][end[1]]
        self.reversed_nodes = self.create_and_connect_nodes(self.reverse_is_passable)
        self.reversed_start = self.reversed_nodes[end[0]][end[1]]

    def create_and_connect_nodes(self, criterion: Callable[List[Tuple[int, int], Tuple[int, int]], bool]) -> List[List[Node]]:
        """Creates and connects the matrix of nodes, based on the criterion"""
        nodes = [[Node([None, None, None, None], (i, j)) for j in range(self.dimensions[1])] for i in range(self.dimensions[0])]
        for i in range(self.dimensions[0]):
            if criterion((i, 0), (i, 1)):
                nodes[i][0].neighbours[1] = nodes[i][1]

            for j in range(1, self.dimensions[1] - 1):
                if criterion((i, j), (i, j - 1)):
                    nodes[i][j].neighbours[3] = nodes[i][j - 1]
                if criterion((i, j), (i, j + 1)):
                    nodes[i][j].neighbours[1] = nodes[i][j + 1]

            if criterion((i, self.dimensions[1] - 1), (i, self.dimensions[1] - 2)):
                nodes[i][self.dimensions[1] - 1].neighbours[3] = nodes[i][self.dimensions[1] - 2]

        # 2. Do vertical scan
        for j in range(self.dimensions[1]):
            if criterion((0, j), (1, j)):
                nodes[0][j].neighbours[0] = nodes[1][j]

            for i in range(1, self.dimensions[0] - 1):
                if criterion((i, j), (i - 1, j)):
                    nodes[i][j].neighbours[2] = nodes[i - 1][j]
                if criterion((i, j), (i + 1, j)):
                    nodes[i][j].neighbours[0] = nodes[i + 1][j]

            if criterion((self.dimensions[0] - 1, j), (self.dimensions[0] - 2, j)):
                nodes[self.dimensions[0] - 1][j].neighbours[0] = nodes[self.dimensions[0] - 2][j]

        return nodes

    def dijkstra(self) -> int: # NOTE: see: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
        """Runs dijkstras algorithm to find the shortest path."""
        unvisited: List[Node] = sum(self.nodes, [])
        # Step 2
        current = self.start
        current.distance = 0

        while len(unvisited) > 0:
            for neighbour in current.neighbours:
                # Step 3
                if neighbour is not None and neighbour in unvisited:
                    tenative_distance_through_current_node = current.distance + 1
                    if neighbour.distance > tenative_distance_through_current_node:
                        neighbour.distance = tenative_distance_through_current_node

            unvisited.remove(current)

            if len(unvisited) == 0:
                break

            node_with_min_distance = sorted(unvisited, key=lambda node: node.distance)[0]
            if self.end not in unvisited or node_with_min_distance.distance == np.inf:
                break

            current = node_with_min_distance

        return self.end.distance

    def reversed_dijkstra(self) -> int: # NOTE: see: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
        """Runs dijkstras algorithm to find the shortest path."""
        unvisited: List[Node] = sum(self.reversed_nodes, [])
        # Step 2
        current = self.reversed_start
        current.distance = 0


        while len(unvisited) > 0:
            for neighbour in current.neighbours:
                # Step 3
                if neighbour is not None and neighbour in unvisited:
                    tenative_distance_through_current_node = current.distance + 1
                    if neighbour.distance > tenative_distance_through_current_node:
                        neighbour.distance = tenative_distance_through_current_node

            unvisited.remove(current)

            if len(unvisited) == 0:
                break

            node_with_min_distance = sorted(unvisited, key=lambda node: node.distance)[0]
            i, j = node_with_min_distance.pos
            if self.height_map[i][j] == 'a':
                return node_with_min_distance.distance

            if node_with_min_distance.distance == np.inf:
                break

            current = node_with_min_distance

        return node_with_min_distance

    def get_a (self) -> List[Tuple[int, int]]:
        """Return"""
        return [(i, j) for (i, j) in product(range(self.dimensions[0]), range(self.dimensions[1])) if self.__get_height(i, j) == 0]


    def is_passable(self, frm: Tuple[int, int], to: Tuple[int, int]) -> bool:
        """Returns true if the height difference from the from position to the to position is less or equal to 1"""
        return self.__get_height(frm[0], frm[1]) - self.__get_height(to[0], to[1])  <= 1

    def reverse_is_passable(self, frm: Tuple[int, int], to: Tuple[int, int]) -> bool:
        """Returns true if the height difference from the from position to the to position is less or equal to 1"""
        return self.__get_height(to[0], to[1]) - self.__get_height(frm[0], frm[1])  <= 1

    def distance(self, frm: Tuple[int, int], to: Tuple[int, int]) -> int:
        """Returns the texicab distance"""
        return abs(frm[0] - to[0]) + abs(frm[1] - to[1])

    def __get_height(self, i: int, j: int) -> int:
        """ Returns the height a position i, j"""
        c = self.height_map[i][j]

        if c == 'S':
            return 0
        elif c == 'E':
            return ord('z') - ord('a')

        return ord(c) - ord('a')


def find_indicies_of_char(lines: List[str], c: str) -> Tuple[int, int]:
    """Computes the indicies of the char given the lines."""
    for idx, line in enumerate(lines):
        if c in line:
            return (idx, line.index(c))

if __name__ == "__main__":
    with open(os.path.join(os.getcwd(), "data", "day12.txt"), "r") as file:

        contents = file.read()
        if 'S' not in contents or 'E' not in contents:
            print("WTF")
            exit()

        lines = contents.split("\n")[:-1]
        graph = Graph(lines, find_indicies_of_char(lines, 'S'), find_indicies_of_char(lines, 'E'))

        print(graph.dijkstra())

        print(graph.reversed_dijkstra())
