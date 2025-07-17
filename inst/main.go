package main

import (
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"
)

func printSuccess(message string) {
	fmt.Printf(" \033[32m\u2705 %s\033[0m\n", message)
}

func printFailure(message string) {
	fmt.Printf(" \033[31m\u2717 %s\033[0m\n", message)
}

func printWarning(message string) {
	fmt.Printf(" \033[33m\u2690  %s\033[0m\n", message)
}

func checkR() bool {
	_, err := exec.LookPath("R")
	if err != nil {
		return false
	}
	return true
}

func checkFilePassed() bool {
	return len(os.Args) >= 2
}

func checkFileExists(filename string) bool {
	_, err := os.Stat(filename)
	if err != nil {
		return false
	}
	return true
}

func checkBueller() bool {
	cmd := exec.Command(
		"R",
		"--slave",
		"--no-restore",
		"--no-save",
		"-e",
		"cat(require('bueller', quietly=TRUE))",
	)

	out, err := cmd.Output()
	if err != nil {
		fmt.Printf("Warning: Could not check for bueller package: %v\n", err)
		return false
	}

	if strings.TrimSpace(string(out)) != "TRUE" {
		return false
	}
	return true
}

func installBueller() bool {
	cmd := exec.Command(
		"R",
		"--slave",
		"--no-restore",
		"--no-save",
		"-e",
		"install.packages('remotes', repos='https://cran.r-project.org'); remotes::install_github('erwx/bueller')",
	)

	_, err := cmd.Output()
	if err != nil {
		fmt.Println("Warning: Could not install bueller package.")
		return false
	}
	return true
}

func findRender() (string, error) {
	cmd := exec.Command(
		"R",
		"--slave",
		"--no-restore",
		"--no-save",
		"-e",
		"cat(system.file('bin', 'bueller', package = 'bueller'))",
	)

	out, err := cmd.Output()
	if err != nil {
		return "", err
	}

	p := strings.TrimSpace(string(out))
	return p, nil
}

func runBueller(filepath string, file string, p param, start chan bool, stop chan bool) {
	start <- true
	cmd := exec.Command(
		"Rscript",
		filepath,
		file,
		"--baseline", strconv.Itoa(p.baseline),
		"--reduction", strconv.Itoa(p.reduction),
		"--years", strconv.Itoa(p.years),
	)
	err := cmd.Run()
	if err != nil {
		fmt.Printf("Could not run bueller: %v\n", err)
	}
	stop <- true
}

type param struct {
	baseline  int
	reduction int
	years     int
}

func newParams() *param {
	p := &param{}
	fmt.Printf("Enter the baseline year: ")
	fmt.Scanln(&p.baseline)
	fmt.Printf("Enter the reduction amount: ")
	fmt.Scanln(&p.reduction)
	fmt.Printf("Enter the number of years: ")
	fmt.Scanln(&p.years)
	return p
}

func main() {

	start := make(chan bool)
	stop := make(chan bool)

	spinner := []string{"|", "/", "-", "\\"}

	p := newParams()

	fmt.Println("Checking system requirements...")

	if !checkR() {
		printFailure("R is not installed or not in PATH.")
		os.Exit(1)
	}
	printSuccess("R is already installed!")

	if !checkFilePassed() {
		printFailure("No file provided.")
		os.Exit(1)
	}

	arg := os.Args[1]
	if !checkFileExists(arg) {
		printFailure("The data file you specified doesn't exist.")
		os.Exit(1)
	}
	printSuccess("The data file you specified exists.")

	if !checkBueller() {
		printWarning("The bueller R package is not installed.")
		printWarning("Install it now? [y/n]: ")

		var response string
		fmt.Scanln(&response)
		response = strings.ToLower(response)

		switch response {
		case "y", "yes":
			printWarning("Installing bueller...")
			if !installBueller() {
				os.Exit(1)
			}
			printSuccess("Success!")
			path, _ := findRender()

			go runBueller(path, arg, *p, start, stop)
			<-start
			i := 0
			for {
				select {
				case <-stop:
					fmt.Println("Done!")
					return
				default:
					fmt.Printf("\r%s Working...", spinner[i])
					i = (i + 1) % len(spinner)
					time.Sleep(500 * time.Millisecond)
				}
			}

		case "n", "no":
			printFailure("Can't proceed without necessary packages.")
			os.Exit(0)
		default:
			printFailure("Invalid entry. Bye.")
			os.Exit(1)
		}
		return
	}
	printSuccess("bueller is already installed!")
	path, _ := findRender()

	go runBueller(path, arg, *p, start, stop)
	<-start
	i := 0
	for {
		select {
		case <-stop:
			fmt.Println("Done!")
			return
		default:
			fmt.Printf("\r%s Working...", spinner[i])
			i = (i + 1) % len(spinner)
			time.Sleep(500 * time.Millisecond)
		}
	}
}
