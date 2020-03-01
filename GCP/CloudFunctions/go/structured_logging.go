// Package p contains an HTTP Cloud Function.
package cloudfunctions

import (
	"fmt"
	"github.com/inconshreveable/log15"
	zlog "github.com/rs/zerolog/log"
	"github.com/sirupsen/logrus"
	"html"
	"log"
	"net/http"
	"os"
)

func StructuredLoggingEndpoint(w http.ResponseWriter, r *http.Request) {

	oneLineMessage := "One line message"
	twoLineMessage := `Two line
message`

	log.Println("=== log Package ===")
	log.Println("Println", oneLineMessage)
	log.Println("Println", twoLineMessage)

	log.Print("Print", oneLineMessage)
	log.Print("Print", twoLineMessage)

	log.Printf("Printf: %s", oneLineMessage)
	log.Printf("Printf: %s", twoLineMessage)

	log.Println("=== sirupsen/logrus Package ===")
	logrus.SetFormatter(&logrus.JSONFormatter{})
	logrus.SetOutput(os.Stdout)
	logrus.SetLevel(logrus.DebugLevel)

	logrus.Debug("Debug Basic")
	logrus.Info("Info Basic")
	logrus.Warn("Warn Basic")
	logrus.Error("Error Basic")
	logrus.Info(oneLineMessage)
	logrus.Info(twoLineMessage)
	logrus.WithFields(logrus.Fields{
		"myIntField":             123,
		"myStringField":          "howdy",
		"myMultilineStringField": twoLineMessage,
	}).Info("With Fields Example")

	log.Println("=== inconshreveable/log15 Package ===")
	l15 := log15.New()
	l15.SetHandler(log15.StreamHandler(os.Stdout, log15.JsonFormat()))
	l15.Debug("Debug Basic")
	l15.Info("Debug Basic")
	l15.Warn("Debug Basic")
	l15.Error("Debug Basic")
	l15.Crit("Crit Basic")
	l15.Info(oneLineMessage)
	l15.Info(twoLineMessage)
	l15.Info("With Fields Example", "myIntField", 532, "myStringField", "howdy", "myMultilineStringField", twoLineMessage)

	log.Println("=== rs/zerolog Package ===")
	zlog.Trace().Msg("Trace Basic")
	zlog.Debug().Msg("Debug Basic")
	zlog.Info().Msg("Debug Basic")
	zlog.Warn().Msg("Debug Basic")
	zlog.Error().Msg("Debug Basic")
	zlog.Info().Msg(oneLineMessage)
	zlog.Info().Msg(twoLineMessage)
	zlog.Info().Int("myIntField", 532).Str("myStringField", "howdy").Str("myMultilineStringField", twoLineMessage).Msg("With Fields Example")

	fmt.Fprint(w, html.EscapeString("OK"))
}
