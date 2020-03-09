// Package p contains an HTTP Cloud Function.
package cloudfunctions

import (
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestLogEndpoint(t *testing.T) {

	res := httptest.NewRecorder()
	req := httptest.NewRequest(http.MethodGet, "/doesnotmatter", nil)

	LogEndpoint(res, req)

	if res.Code != http.StatusOK {
		t.Error("Expected status", res.Code, "but got", http.StatusOK)
	}

	bodyStr := res.Body.String()
	if bodyStr != "OK" {
		t.Error("Expected body OK but got", bodyStr)
	}
}
