package main

import (
	"fmt"
	"io"
	"os"
)

func readInput(fname string) ([]int, error) {
	file, err := os.Open(fname)

	if err != nil {
		return nil, err
	}

	var perline int
	var nums []int

	for {
		_, err := fmt.Fscanf(file, "%d\n", &perline)
		if err != nil {
			if err == io.EOF {
				break
			}
			return nil, err
		}

		nums = append(nums, perline)
	}

	return nums, nil

}

func solve(nums []int) int {
	var result int
	for i, num1 := range nums {
		for _, num2 := range nums[i:] {
			if num1+num2 == 2020 {
				result = num1 * num2
			}
		}
	}
	return result
}

func main() {
	nums, err := readInput("input.txt")
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	fmt.Println(solve(nums))
}
