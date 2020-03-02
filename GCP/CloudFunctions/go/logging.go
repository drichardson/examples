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
// Information about how google-fluentd processes logs is here:
// https://cloud.google.com/logging/docs/agent/configuration
//
// Viewing Custom Fields in Stackdriver Web Console
// https://cloud.google.com/logging/docs/view/overview#custom-fields

package cloudfunctions

import (
	"context"
	"flag"
	"fmt"
	"html"
	"log"
	"net/http"
	"os"
	"time"

	"cloud.google.com/go/logging"
	"github.com/golang/glog"
	"github.com/inconshreveable/log15"
	"github.com/rs/zerolog"
	zlog "github.com/rs/zerolog/log"
	"github.com/sirupsen/logrus"
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
	log.SetFlags(0)
	log.Println("Println:", oneLineMessage)
	log.Println("Println:", twoLineMessage+" for log.Println")

	log.Print("Print:", oneLineMessage)
	log.Print("Print:", twoLineMessage+" for log.Print")

	log.Printf("Printf:%s", oneLineMessage)
	log.Printf("Printf:%s", twoLineMessage+" for log.Printf")

	printSectionHeader("github.com/golang/glog")
	// glog relies on flag command line variables to set logging params
	// logging parameters.
	flag.Set("stderrthreshold", "INFO")
	flag.Set("v", "1")
	flag.Parse()
	glog.Info("Info Basic")
	glog.Warning("Warning Basic")
	glog.Error("Error Basic")
	glog.V(0).Info("Info V=0")
	glog.V(1).Info("Info V=1")
	glog.V(2).Info("Info V=2")

	// Flush to make sure glog doesn't interfere with logs that come later.
	// This needs to be the last glog function called.
	glog.Flush()

	printSectionHeader("github.com/sirupsen/logrus JSONFormatter")
	logrus.SetFormatter(&logrus.JSONFormatter{
		TimestampFormat: time.RFC3339Nano,
		// set field keys to be compatible with stackdriver
		FieldMap: logrus.FieldMap{
			logrus.FieldKeyTime:  "timestamp",
			logrus.FieldKeyLevel: "severity",
			logrus.FieldKeyMsg:   "message",
		},
	})
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

	printSectionHeader("github.com/sirupsen/logrus TextFormatter")
	logrus.SetFormatter(&logrus.TextFormatter{
		DisableColors:   true,
		TimestampFormat: time.RFC3339Nano,
		FieldMap: logrus.FieldMap{
			logrus.FieldKeyTime:  "timestamp",
			logrus.FieldKeyLevel: "severity",
			logrus.FieldKeyMsg:   "message",
		},
	})
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

	printSectionHeader("github.com/inconshreveable/log15 Package")
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

	printSectionHeader("github.com/rs/zerolog Package")
	zerolog.TimeFieldFormat = time.RFC3339Nano
	// set field names to be compatible with stackdriver
	zerolog.LevelFieldName = "severity"      // for stackdriver
	zerolog.TimestampFieldName = "timestamp" // for stackdriver
	zlog.Trace().Msg("Trace Basic")
	zlog.Debug().Msg("Debug Basic")
	zlog.Info().Msg("Info Basic")
	zlog.Warn().Msg("Warn Basic")
	zlog.Error().Msg("Error Basic")
	zlog.Info().Msg(oneLineMessage)
	zlog.Info().Msg(twoLineMessage + " for zlog.Info")
	zlog.Info().Int("myIntField", 532).Str("myStringField", "howdy").Str("myMultilineStringField", twoLineMessage+" for zlog.Info with fields").Msg("With Fields Example")

	printSectionHeader("cloud.google.com/go/logging")
	projectId := os.Getenv("GCP_PROJECT")
	if client, err := logging.NewClient(context.Background(), "projects/"+projectId); err == nil {
		lg := client.Logger("my-log")
		lg.Log(logging.Entry{Payload: oneLineMessage})
		lg.Log(logging.Entry{Payload: twoLineMessage + " for cloud.google.com/go/logging"})
		lg.Log(logging.Entry{Severity: logging.Debug, Payload: "Debug"})
		lg.Log(logging.Entry{Severity: logging.Info, Payload: "Info"})
		lg.Log(logging.Entry{Severity: logging.Notice, Payload: "Notice"})
		lg.Log(logging.Entry{Severity: logging.Warning, Payload: "Warning"})
		lg.Log(logging.Entry{Severity: logging.Error, Payload: "Error"})
		lg.Log(logging.Entry{Severity: logging.Critical, Payload: "Critial"})
		lg.Log(logging.Entry{Severity: logging.Alert, Payload: "Alert"})
		lg.Log(logging.Entry{Severity: logging.Emergency, Payload: "Emergency"})
		lg.Log(logging.Entry{
			Severity: logging.Debug,
			Payload: map[string]interface{}{
				"intVal":        123,
				"oneLineString": oneLineMessage,
				"twoLineString": twoLineMessage + " for cloud.google.com/go/logging with fields.",
			},
		})

		err = client.Close()
		if err != nil {
			panic(err)
		}
	} else {
		log.Println("WARNING: Skipping cloud.google.com/go/logging examples because logging.NewClient failed.", err)
	}

	fmt.Fprint(w, html.EscapeString("OK"))
}
