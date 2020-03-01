// Package p contains an HTTP Cloud Function.
package cloudfunctions

import (
	"fmt"
	"html"
	"log"
	"net/http"
)

func StructuredLoggingEndpoint(w http.ResponseWriter, r *http.Request) {

	oneLineMessage := "One line message"
	twoLineMessage := `Two line
message`

	log.Println("Println", oneLineMessage)
	log.Println("Println", twoLineMessage)

	log.Print("Print", oneLineMessage)
	log.Print("Print", twoLineMessage)

	log.Printf("Printf: %s", oneLineMessage)
	log.Printf("Printf: %s", twoLineMessage)

	fmt.Fprint(w, html.EscapeString("OK"))
}
