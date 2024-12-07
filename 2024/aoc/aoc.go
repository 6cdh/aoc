package aoc

import (
	"aoc2024/iter"
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
		"    run           run today's solution with your input.",
		"    run <day>     run <day>'s solution with your input.",
		"    run all       run all solutions with your inputs.",
		"    test          run today's solution, read input from stdin.",
		"    test <day>    run <day>'s solution, read input from stdin.",
		"    submit        run and submit today's next answer.",
		"    submit <day>  run and submit <day>'s next answer.",
		"<day> is an integer in range",
		fmt.Sprintf("    1 ≤ day ≤ %d", aoc.maxDay),
		"subcommand shortcuts:",
		"    r -> run",
		"    t -> test",
		"    s -> submit",
	}
	fmt.Println(strings.Join(helpMsg, "\n"))
}

func (aoc *AOC) guessToday() int {
	utcMinus5 := time.FixedZone("UTC-5", -5*60*60)
	now := time.Now().In(utcMinus5)
	if !(now.Year() == 2024 && now.Month() == 12) {
		log.Fatal("It's not December 2024, can't guess today's problem.")
	}
	return now.Day()
}

func (aoc *AOC) parseDay(str string) int {
	day, err := strconv.Atoi(str)
	if err != nil {
		log.Fatalf("Invalid <day> '%s': not an integer.", str)
	}
	if !(1 <= day && day <= aoc.maxDay) {
		log.Fatalf("Invalid <day> '%d': out of range.", day)
	}
	return day
}

// runDay run day's solution with reader `in`.
func (aoc *AOC) runDay(day int, in io.ReadCloser, out io.Writer) {
	defer in.Close()
	start := time.Now()
	aoc.solve(day, in, out)
	fmt.Fprintf(os.Stderr, "Execution time: %v\n", time.Since(start))
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
	if !IsPathExists(filename) {
		println("Input your cookie (from browser):")
		cookie, err := utils.ReadLine(os.Stdin)
		log.FatalIfErr(err)
		err = os.WriteFile(filename, cookie, 0600)
		log.FatalIfErr(err)
		return string(cookie)
	} else if IsPathDir(filename) {
		log.Fatalf("'%s' is a directory, cannnot use it as a cookie file.", filename)
	}
	cookie, err := os.ReadFile(filename)
	log.FatalIfErr(err)
	return string(cookie)
}

func (aoc *AOC) sendHTTP(method string, url string, headers map[string]string, data string) []byte {
	req, err := http.NewRequest(method, url, strings.NewReader(data))
	log.FatalIfErr(err)

	for k, v := range headers {
		req.Header.Set(k, v)
	}
	req.Header.Set("Cookie", aoc.readCookie())

	resp, err := http.DefaultClient.Do(req)
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
		log.Fatalf("'%s' is a directory, cannot to write input data to it.", p)
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
		fmt.Printf("day %d is already completed!\n", day)
		return
	}

	var buffer bytes.Buffer
	aoc.runDay(day, aoc.myInputReader(day), &buffer)
	lines := make([]string, 0)
	for line := range iter.ReadStringLines(&buffer) {
		lines = append(lines, line)
	}
	answer := lines[level-1]

	fmt.Println("submit answer", answer)
	submitUrl := fmt.Sprintf("https://adventofcode.com/2024/day/%d/answer", day)
	form := map[string]string{
		"level":  strconv.Itoa(level),
		"answer": answer,
	}
	response := aoc.httpPOST(submitUrl, form)

	if bytes.Contains(response, []byte("That's the right answer!")) {
		fmt.Println("Answer is correct!")
	} else if bytes.Contains(response, []byte("your answer is too high.")) {
		fmt.Println("Answer is too high.")
	} else if bytes.Contains(response, []byte("your answer is too low.")) {
		fmt.Println("Answer is too low.")
	} else {
		fmt.Println("Unknown response.")
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
	case "run", "r":
		aoc.cmdRun()
	case "test", "t":
		aoc.cmdTest()
	case "submit", "s":
		aoc.cmdSubmit()
	default:
		log.Info("Invalid command.")
		aoc.ShowHelp()
	}
}
