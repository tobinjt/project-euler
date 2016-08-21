/*
* This file contains Project Euler code that I can't test.  The sole purpose is
* to keep coverage of project-euler.go high.
 */

package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"runtime/pprof"
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
