package strings

import (
	"fmt"
	// "regexp"
)

// var fsFormatRegexp = regexp.MustCompile(`(^|[^%])%([0+\- ]*)(\d+)?(?:\.(\d+))?(\w)`)
// var interpolateRegexp = regexp.MustCompile(`(?:(^|[^%])%([0+\- ]*)(\d+)?(?:\.(\d+))?(\w))?%P\(\)`)
// var formatRegexp = regexp.MustCompile(`\{(\d+)(,-?\d+)?(?:\:([a-zA-Z])(\d{0,2})|\:(.+?))?\}`)

// type IPrintfFormat struct {
// 	Input string
// 	Cont  func(func(string) interface{}) func(string) interface{}
// }

// func FormatReplacement(rep interface{}, flags string, padLength string, precision string, format string) string {
// 	sign := ""
// 	if flags == "" {
// 		flags = ""
// 	}
// 	if format == "" {
// 		format = ""
// 	}

// 	if rep.(int) < 0 {
// 		rep = rep.(int) * -1
// 	}

// 	return ret
// }

// func FormatOnce(str string, rep interface{}) string {
// 	ret := fsFormatRegexp.ReplaceAllStringFunc(str, func(m string) string {
// 		prefix, flags, padLength, precision, format := m.Groups()
// 		once := FormatReplacement(rep, flags, padLength, precision, format)
// 		return prefix + once.Replace("%", "%%")
// 	})
// 	return ret
// }

// func CreatePrinter(str string, cont func(string) interface{}) func(...interface{}) interface{} {
// 	return func(args ...interface{}) interface{} {
// 		strCopy := str
// 		for _, arg := range args {
// 			strCopy = FormatOnce(strCopy, arg)
// 		}

// 		if fsFormatRegexp.MatchString(strCopy) {
// 			return CreatePrinter(strCopy, cont)
// 		}
// 		return cont(strCopy)
// 	}
// }

// func FsFormat(str string) func(func(string) interface{}) func(...interface{}) interface{} {
// 	return func(cont func(string) interface{}) func(...interface{}) interface{} {
// 		if fsFormatRegexp.MatchString(str) {
// 			return CreatePrinter(str, cont)
// 		}
// 		return cont(str)
// 	}
// }

// func Printf(input string) IPrintfFormat {
// 	format := FsFormat(input)
// 	return IPrintfFormat{Input: input, Cont: format}
// }

func Format(str string, args ...interface{}) string {
	return fmt.Sprintf(str)
}
