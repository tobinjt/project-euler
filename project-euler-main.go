/*
* This file contains Project Euler code that I can't test.  The sole purpose is
* to keep coverage of other project-euler-*.go files high, so I don't let their
* coverage slip.
 */

package main

import (
	"errors"
	"flag"
	"fmt"
	"log"
	"os"
	"runtime"
	"runtime/pprof"
	"sort"
	"strings"
)

var cpuprofile = flag.String("cpuprofile", "", "write cpu profile to file")
var memprofile = flag.String("memprofile", "", "write memory profile to this file")

func main() {
	flag.Parse()
	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			log.Fatal(err)
		}
		pprof.StartCPUProfile(f)
		defer pprof.StopCPUProfile()
	}
	if *memprofile != "" {
		// Collect stats on all allocations.  This will be slow.
		runtime.MemProfileRate = 1
	}
	result, err := realMain(flag.Args())
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println(result)
	// TODO(johntobin): is this too late?  Have we cleaned up the memory we allocated?
	if *memprofile != "" {
		f, err := os.Create(*memprofile)
		if err != nil {
			log.Fatal(err)
		}
		pprof.WriteHeapProfile(f)
		f.Close()
	}
}

// A function for ad-hoc code during development.
func test() int64 {
	return int64(0)
}

// A dummy function to be called during testing of realMain.
func fortesting() int64 {
	return 0
}

func realMain(args []string) (int64, error) {
	functions := map[string]func() int64{
		"fortesting": fortesting,
		"test":       test,
		"67":         projectEuler67,
		"68":         projectEuler68,
		"69":         projectEuler69,
		"70":         projectEuler70,
		"71":         projectEuler71,
		"72":         projectEuler72,
		"73":         projectEuler73,
		"74":         projectEuler74,
		"75":         projectEuler75,
		"76":         projectEuler76,
		"77":         projectEuler77,
		"78":         projectEuler78,
		"80":         projectEuler80,
		"81":         projectEuler81,
		"82":         projectEuler82,
		"83":         projectEuler83,
		"84":         projectEuler84,
		"85":         projectEuler85,
		"87":         projectEuler87,
		"88":         projectEuler88,
		"89":         projectEuler89,
		"92":         projectEuler92,
		"97":         projectEuler97,
		"99":         projectEuler99,
		"205":        projectEuler205,
		"206":        projectEuler206,
		"357":        projectEuler357,
		"387":        projectEuler387,
		"493":        projectEuler493,
		"549":        projectEuler549,
		"549_2":      projectEuler549_2,
		"549_3":      projectEuler549_3,
		// newPE - this is where the next main entry will go.
	}
	if len(args) != 1 || functions[args[0]] == nil {
		keys := []string{}
		for key := range functions {
			if key != "fortesting" {
				keys = append(keys, key)
			}
		}
		sort.Strings(keys)
		return 0, errors.New("Only 1 arg accepted from this list: " +
			strings.Join(keys, " "))
	}
	return functions[args[0]](), nil
}

func breakpoint() string {
	return fmt.Sprint("breakpoint reached")
}

func projectEuler70() int64 {
	return projectEuler70actual(10 * 1000 * 1000)
}

func projectEuler71() int64 {
	return projectEuler71actual(1000000)
}

func projectEuler72() int64 {
	return projectEuler72actual(1000000)
}

func projectEuler73() int64 {
	return projectEuler73actual(12000)
}

func projectEuler74() int64 {
	return projectEuler74actual(1000000)
}

func projectEuler75() int64 {
	return projectEuler75Actual(1500000)
}

func projectEuler76() int64 {
	return int64(NumIntegerPartitions(100, 99))
}

func projectEuler77() int64 {
	return projectEuler77actual(5000)
}

func projectEuler78() int64 {
	return projectEuler78actual(1000 * 1000)
}

func projectEuler80() int64 {
	return projectEuler80actual(99)
}

func projectEuler81() int64 {
	return projectEuler81actual(openOrDie("matrix.txt"))
}

func projectEuler82() int64 {
	matrix := TwoDAStar82{costs: readIntsFromCSVFile(openOrDie("matrix.txt"))}
	return projectEuler82actual(&matrix)
}

func projectEuler83() int64 {
	matrix := TwoDAStar83{costs: readIntsFromCSVFile(openOrDie("matrix.txt"))}
	return projectEuler83actual(&matrix)
}

func projectEuler84() int64 {
	return projectEuler84actual(4)
}

func projectEuler85() int64 {
	return projectEuler85actual()
}

func projectEuler87() int64 {
	return projectEuler87actual(50 * 1000 * 1000)
}

func projectEuler88() int64 {
	return projectEuler88actual(12000)
}

func projectEuler89() int64 {
	return projectEuler89actual(openOrDie("roman.txt"))
}

func projectEuler92() int64 {
	return projectEuler92actual(10 * 1000 * 1000)
}

func projectEuler97() int64 {
	return projectEuler97actual(7830457, 28433)
}

func projectEuler99() int64 {
	return projectEuler99actual(openOrDie("base_exp.txt"))
}

func projectEuler205() int64 {
	return projectEuler205actual()
}

func projectEuler357() int64 {
	return projectEuler357actual(100 * 1000 * 1000)
}

func projectEuler387() int64 {
	return projectEuler387actual(100 * 1000 * 1000 * 1000 * 1000)
}

func projectEuler549() int64 {
	return projectEuler549actual(100 * 1000 * 1000)
}

func projectEuler549_2() int64 {
	return projectEuler549_2actual(100 * 1000 * 1000)
}

func projectEuler549_3() int64 {
	return projectEuler549_3actual(100 * 1000 * 1000)
}
