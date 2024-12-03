package aoc

import (
	"aoc2024/log"
	"aoc2024/utils"
	"bytes"
	"fmt"
	"io"
	"net/http"
	neturl "net/url"
	"os"
	"path"
	"strconv"
	"strings"
	"time"
)

type Solver = func(id int, in io.Reader, out io.Writer)

type AOC struct {
	solve  Solver
	maxDay int
}

func New(run Solver, maxDay int) *AOC {
	return &AOC{
		solve:  run,
		maxDay: maxDay,
	}
}

func (aoc *AOC) ShowHelp() {
	helpMsg := []string{
		"aoc2024 is a tool to manage advent of code solutions.",
		"Usage:",
		"    aoc2024 <command>",
		"<command> is one of",
		"    run           run today's solution for your input",
		"    run <day>     run <day>'s solution for your input",
		"    run all       run all solutions for your inputs",
		"    test          run today's solution, read from stdin",
		"    test <day>    run <day>'s solution, read from stdin",
		"    submit        run and submit today's next answer",
		"    submit <day>  run and submit <day>'s next answer",
		"<day> is an integer in range",
		fmt.Sprintf("    1 ≤ day ≤ %d", aoc.maxDay),
	}
	fmt.Println(strings.Join(helpMsg, "\n"))
}

func (aoc *AOC) guessToday() int {
	now := time.Now()
	utcMinus5 := time.FixedZone("UTC-5", -5*60*60)
	nowUTCMinus5 := now.In(utcMinus5)
	if !(nowUTCMinus5.Year() == 2024 && nowUTCMinus5.Month() == 12) {
		log.Fatal("it's not in December 2024, can't guess today's problem id")
	}
	return nowUTCMinus5.Day()
}

func (aoc *AOC) parseDay(str string) int {
	day, err := strconv.Atoi(str)
	if err != nil {
		log.Fatalf("error when parse <day> \"%s\": not an integer", str)
	}
	if !(1 <= day && day <= aoc.maxDay) {
		log.Fatalf("error when parse <day> \"%s\": out of range", str)
	}
	return day
}

// runDay run day's solution with reader `in`.
func (aoc *AOC) runDay(day int, in io.ReadCloser, out io.Writer) {
	defer in.Close()
	start := time.Now()
	aoc.solve(day, in, out)
	spend := time.Since(start)
	fmt.Fprintf(os.Stderr, "%v\n", spend)
}

func IsPathExists(p string) bool {
	_, err := os.Stat(p)
	return !os.IsNotExist(err)
}

func IsPathDir(p string) bool {
	info, err := os.Stat(p)
	return err == nil && info.IsDir()
}

func (aoc *AOC) readCookie() string {
	filename := ".cookie"
	var cookie []byte
	if !IsPathExists(filename) {
		println("Input your cookie (from browser):")
		var err error
		cookie, err = utils.ReadLine(os.Stdin)
		log.FatalIfErr(err)
		err = os.WriteFile(filename, cookie, 0644)
		log.FatalIfErr(err)
	} else if IsPathDir(filename) {
		log.Fatalf("'%s' is a directory, aoc failed to write cookie into it", filename)
	} else {
		var err error
		cookie, err = os.ReadFile(filename)
		log.FatalIfErr(err)
	}
	return string(cookie)
}

func (aoc *AOC) sendHTTP(method string, url string, headers map[string]string, data string) []byte {
	client := &http.Client{}
	req, err := http.NewRequest(method, url, strings.NewReader(data))
	log.FatalIfErr(err)
	for k, v := range headers {
		req.Header.Set(k, v)
	}
	req.Header.Set("Cookie", aoc.readCookie())
	resp, err := client.Do(req)
	log.FatalIfErr(err)
	defer resp.Body.Close()

	content, err := io.ReadAll(resp.Body)
	log.FatalIfErr(err)
	return content
}

func (aoc *AOC) httpGET(url string) []byte {
	return aoc.sendHTTP("GET", url, nil, "")
}

func (aoc *AOC) httpPOST(url string, form map[string]string) []byte {
	fd := neturl.Values{}
	for k, v := range form {
		fd.Set(k, v)
	}
	headers := map[string]string{"Content-Type": "application/x-www-form-urlencoded"}
	return aoc.sendHTTP("POST", url, headers, fd.Encode())
}

func (aoc *AOC) fetchInput(day int) []byte {
	fmt.Printf("Fetching Input for day %d ...\n", day)
	url := fmt.Sprintf("https://adventofcode.com/2024/day/%d/input", day)
	return aoc.httpGET(url)
}

func (aoc *AOC) myInputReader(day int) io.ReadCloser {
	inputDir := "input"
	filename := fmt.Sprintf("day%02d.txt", day)
	p := path.Join(inputDir, filename)
	if !IsPathExists(p) {
		input := aoc.fetchInput(day)
		// ignore error if inputDir exists
		os.Mkdir(inputDir, 0755)
		err := os.WriteFile(p, input, 0644)
		log.FatalIfErr(err)
	} else if IsPathDir(p) {
		log.Fatalf("'%s' is a directory, aoc failed to write input data into it", p)
	}
	in, err := os.Open(p)
	log.FatalIfErr(err)
	return in
}

func (aoc *AOC) cmdRun() {
	if len(os.Args) == 2 {
		day := aoc.guessToday()
		aoc.runDay(day, aoc.myInputReader(day), os.Stdout)
	} else if os.Args[2] == "all" {
		for day := 1; day <= aoc.maxDay; day++ {
			aoc.runDay(day, aoc.myInputReader(day), os.Stdout)
		}
	} else {
		day := aoc.parseDay(os.Args[2])
		aoc.runDay(day, aoc.myInputReader(day), os.Stdout)
	}
}

func (aoc *AOC) cmdTest() {
	if len(os.Args) != 3 {
		aoc.runDay(aoc.guessToday(), os.Stdin, os.Stdout)
	} else {
		aoc.runDay(aoc.parseDay(os.Args[2]), os.Stdin, os.Stdout)
	}
}

func (aoc *AOC) submitDay(day int) {
	problemUrl := fmt.Sprintf("https://adventofcode.com/2024/day/%d", day)
	page := aoc.httpGET(problemUrl)
	level := 1
	if bytes.Contains(page, []byte("--- Part Two ---")) {
		level = 2
	}
	if bytes.Contains(page, []byte("Both parts of this puzzle are complete!")) {
		fmt.Printf("day %d is completed!\n", day)
		return
	}

	var buffer bytes.Buffer
	aoc.runDay(day, aoc.myInputReader(day), &buffer)
	lines := make([]string, 0)
	for line := range utils.ReadStringLines(&buffer) {
		lines = append(lines, line)
	}
	submitUrl := fmt.Sprintf("https://adventofcode.com/2024/day/%d/answer", day)
	answer := lines[level-1]
	fmt.Println("submit answer", answer)
	form := map[string]string{
		"level":  fmt.Sprintf("%d", level),
		"answer": answer,
	}
	resp := aoc.httpPOST(submitUrl, form)
	if bytes.Contains(resp, []byte("That's the right answer!")) {
		fmt.Println("pass!")
	} else {
		fmt.Println("failed!")
		if bytes.Contains(resp, []byte("your answer is too high.")) {
			fmt.Println("too high!")
		} else if bytes.Contains(resp, []byte("your answer is too low.")) {
			fmt.Println("too low!")
		} else {
			fmt.Println("can't determine if it's low or high.")
		}
	}
}

func (aoc *AOC) cmdSubmit() {
	var day int
	if len(os.Args) == 2 {
		day = aoc.guessToday()
	} else {
		day = aoc.parseDay(os.Args[2])
	}
	aoc.submitDay(day)
}

func (aoc *AOC) ParseAndRun() {
	if len(os.Args) < 2 {
		aoc.ShowHelp()
		return
	}
	switch os.Args[1] {
	case "run":
		aoc.cmdRun()
	case "test":
		aoc.cmdTest()
	case "submit":
		aoc.cmdSubmit()
	default:
		log.Info("wrong command")
		aoc.ShowHelp()
	}
}
