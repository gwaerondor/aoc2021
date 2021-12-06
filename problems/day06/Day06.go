package main

import (
	"aoc/lib"
	"fmt"
)

func main() {
	in := lib.IntOfCsvOfFileOfDay(6)[0]
	p1 := Day06Part1(in)
	p2 := Day06Part2(in)
	fmt.Printf("Part 1: %d\nPart 2: %d\n", p1, p2)
}

func Day06Part1(fish []int) int {
	fishMap := parseInitState(fish)
	for range lib.Seq(1, 80) {
		fishMap = tick(fishMap)
	}
	return totalFish(fishMap)
}

func Day06Part2(fish []int) int {
	fishMap := parseInitState(fish)
	for range lib.Seq(1, 256) {
		fishMap = tick(fishMap)
	}
	return totalFish(fishMap)
}

func parseInitState(fish []int) map[int]int {
	fishMap := map[int]int{0:0, 1:0, 2:0, 3:0, 4:0, 5:0, 6:0, 7:0, 8:0}
	for _, e := range fish {
		fishMap[e]++
	}
	return fishMap
}

func totalFish(fish map[int]int) int {
	sum := 0
	for _, v := range fish {
		sum += v
	}
	return sum
}

func tick(fish map[int]int) map[int]int {
	newlySpawned := fish[0]
	for i := 1; i <= 8; i++ {
		fish[i-1] = fish[i]
	}
	fish[8] = newlySpawned
	fish[6] += newlySpawned
	return fish
}
