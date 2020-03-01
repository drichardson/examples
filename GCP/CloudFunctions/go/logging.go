//
// Example used to demonstrate how logging (structured and unstructured) works on
// GCP. Deploy the cloud functions, and then eopen the Stackdriver Logging web
// console to see how the various mechanisms of logging integrate with
// [Stackdriver Logging web console](https://console.cloud.google.com/logs)
//
// For example:
//
//    1. Are logging levels (debug, warn, error, etc) interpreted correctly
//       by Stackdriver?
//    2. Is the main message printed out as the first line in Stackdriver?
//    3. Are structured log fields queryable in Stackdriver?
//    4. Are logs ordered correctly in Stackdriver? If not, check timestamp
//       resolution and name.
//

package cloudfunctions

import (
	"fmt"
	"github.com/inconshreveable/log15"
	"github.com/rs/zerolog"
	zlog "github.com/rs/zerolog/log"
	"github.com/sirupsen/logrus"
	"html"
	"log"
	"net/http"
	"os"
	"time"
)

func printSectionHeader(section string) {
	os.Stdout.WriteString("===" + section + "===\n")
}

func LogEndpoint(w http.ResponseWriter, r *http.Request) {

	oneLineMessage := "One line message"
	twoLineMessage := `Line 1 of 2
Line 2 of 2`

	printSectionHeader("fmt")
	fmt.Print("Print:", oneLineMessage, "\n")
	fmt.Print("Print:", twoLineMessage+" for fmt.Print\n")
	fmt.Println("Println:", oneLineMessage)
	fmt.Println("Println:", twoLineMessage+" for fmt.Println")
	fmt.Printf("Printf:%s\n", oneLineMessage)
	fmt.Printf("Printf:%s\n", twoLineMessage+" for fmt.Printf")

	printSectionHeader("os")
	os.Stdout.WriteString("WriteString:" + oneLineMessage + "\n")
	os.Stdout.WriteString("WriteString:" + twoLineMessage + " for WriteString\n")

	printSectionHeader("log")
	log.SetFlags(log.Flags() | log.Lmicroseconds)
	log.Println("Println:", oneLineMessage)
	log.Println("Println:", twoLineMessage+" for log.Println")

	log.Print("Print:", oneLineMessage)
	log.Print("Print:", twoLineMessage+" for log.Print")

	log.Printf("Printf:%s", oneLineMessage)
	log.Printf("Printf:%s", twoLineMessage+" for log.Printf")

	printSectionHeader("sirupsen/logrus")
	logrus.SetFormatter(&logrus.JSONFormatter{})
	logrus.SetOutput(os.Stdout)
	logrus.SetLevel(logrus.DebugLevel)

	logrus.Debug("Debug Basic")
	logrus.Info("Info Basic")
	logrus.Warn("Warn Basic")
	logrus.Error("Error Basic")
	logrus.Info(oneLineMessage)
	logrus.Info(twoLineMessage + " for logrus.Info")
	logrus.WithFields(logrus.Fields{
		"myIntField":             123,
		"myStringField":          "howdy",
		"myMultilineStringField": twoLineMessage + " for logrus.WithFields.Info",
	}).Info("With Fields Example")

	printSectionHeader("inconshreveable/log15 Package")
	l15 := log15.New()
	l15.SetHandler(log15.StreamHandler(os.Stdout, log15.JsonFormat()))
	l15.Debug("Debug Basic")
	l15.Info("Debug Basic")
	l15.Warn("Debug Basic")
	l15.Error("Debug Basic")
	l15.Crit("Crit Basic")
	l15.Info(oneLineMessage)
	l15.Info(twoLineMessage + " for l15.Info")
	l15.Info("With Fields Example", "myIntField", 532, "myStringField", "howdy", "myMultilineStringField", twoLineMessage+" for l15.Info with fields")

	printSectionHeader("rs/zerolog Package")
	//zerolog.TimestampFieldName = "t"
	//zerolog.LevelFieldName = "l"
	//zerolog.MessageFieldName = "m"
	zerolog.TimeFieldFormat = time.RFC3339Nano
	zlog.Trace().Msg("Trace Basic")
	zlog.Debug().Msg("Debug Basic")
	zlog.Info().Msg("Debug Basic")
	zlog.Warn().Msg("Debug Basic")
	zlog.Error().Msg("Debug Basic")
	zlog.Info().Msg(oneLineMessage)
	zlog.Info().Msg(twoLineMessage + " for zlog.Info")
	zlog.Info().Int("myIntField", 532).Str("myStringField", "howdy").Str("myMultilineStringField", twoLineMessage+" for zlog.Info with fields").Msg("With Fields Example")

	fmt.Fprint(w, html.EscapeString("OK"))
}
