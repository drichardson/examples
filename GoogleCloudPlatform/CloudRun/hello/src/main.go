package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"net/http"
	"os"
	"strconv"
	"time"

	"github.com/rs/zerolog"
)

// Defaulting to 6666 just to show that you really need to look at the PORT
// environment variable set by Google Cloud Run in order to know what port to
// use.
var portFlag = flag.Int("port", 6666, "Listening port. Overriden by PORT environment variable.")

var logLevel = flag.String("loglevel", "", "zerolog debug level")
var logConsole = flag.Bool("logconsole", false, "Use pretty console logging, instead of structured logging.")

func main() {
	flag.Parse()
	initLogging()

	log, _ := newLogger(context.Background())

	http.HandleFunc("/", handler)

	// Per Google Cloud Run Container runtime contract, you must listen for
	// requests on the PORT environment variable. For more information, see
	// https://cloud.google.com/run/docs/reference/container-contract
	port := *portFlag
	if portEnv := os.Getenv("PORT"); portEnv != "" {
		var err error
		port, err = strconv.Atoi(portEnv)
		if err != nil {
			log.Fatal().AnErr("error", err).Str("port", portEnv).Msg("Failed to parse PORT environment variable")
		}
	}

	log.Info().Int("port", port).Msg("Listening")
	if err := http.ListenAndServe(fmt.Sprintf(":%d", port), nil); err != nil {
		log.Fatal().AnErr("error", err).Msg("ListenAndServe failed")
	}
}

func handler(w http.ResponseWriter, r *http.Request) {
	log, _ := newLogger(r.Context())

	log.Debug().Msg("handler called")

	w.Header().Add("Content-Type", "text/plain; charset=utf-8")
	w.WriteHeader(http.StatusOK)

	if _, err := w.Write([]byte("Hello, World!\n")); err != nil {
		log.Err(err).Msg("Write failed")
	} else {
		log.Debug().Msg("Write succeeded")
	}
}

// newLogger returns a zerolog.Logger that logs to standard output, which
// is how you log application data in Google Kubernetes Engine, Cloud Functions,
// and Cloud Run.
//
// This logger can also be retreived with zerolog.Ctx().
func newLogger(ctx context.Context) (*zerolog.Logger, context.Context) {
	//logger := zerolog.New(os.Stdout)
	logger := zerolog.New(logWriter)
	l := &logger
	return &logger, l.WithContext(ctx)
}

var logWriter io.Writer = os.Stdout

func initLogging() {
	// To play nicely with Google Cloud Platform logging
	// https://cloud.google.com/run/docs/logging#container-logs
	zerolog.LevelFieldName = "severity"
	zerolog.MessageFieldName = "message"

	// https://cloud.google.com/logging/docs/agent/configuration#timestamp-processing
	zerolog.TimestampFieldName = "time"
	zerolog.TimeFieldFormat = time.RFC3339Nano

	zerolog.SetGlobalLevel(zerolog.InfoLevel)

	lvl := *logLevel
	if lvl == "" {
		lvl = os.Getenv("LOG_LEVEL")
	}
	if lvl != "" {
		if level, err := zerolog.ParseLevel(lvl); err == nil {
			zerolog.SetGlobalLevel(level)
		} else {
			fmt.Fprintln(os.Stderr, "Error parsing LOG_LEVEL environment variable in init. LOG_LEVEL =", logLevel, "err = ", err)
		}
	}

	if *logConsole {
		logWriter = zerolog.NewConsoleWriter()
	}
}
