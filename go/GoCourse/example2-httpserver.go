package main

import (
	"http"
	"os"
	"fmt"
)

type Rot13ResponseWriter struct {
	writer http.ResponseWriter;
}

func (w *Rot13ResponseWriter) Header() http.Header {
	return w.writer.Header()
}

func (w *Rot13ResponseWriter) Write(bytes []byte) (int, os.Error) {
	
	for i := len(bytes) - 1; i >= 0; i-- {
		if (bytes[i] >= 'a' && bytes[i] <= 'z') {
			bytes[i] = (((bytes[i] - 'a') + 13) % 26) + 'a'
		} else if (bytes[i] >= 'A' && bytes[i] <= 'Z') {
			bytes[i] = (((bytes[i] - 'A') + 13) % 26) + 'A'
		}
	}
	
	return w.writer.Write(bytes)
}

func (w *Rot13ResponseWriter) WriteHeader(i int) {
	w.writer.WriteHeader(i)
}

type Rot13FileServerHandler struct {
	h http.Handler
}

func (h *Rot13FileServerHandler) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	var rot13Writer *Rot13ResponseWriter = new(Rot13ResponseWriter);
	rot13Writer.writer = w
	h.h.ServeHTTP(rot13Writer, req)
}

func main() {
	wd, _ := os.Getwd()
	
	handler := new(Rot13FileServerHandler);
	handler.h = http.FileServer(wd + "/httpdoc", "")
	
	err := http.ListenAndServe(":12345", handler)
	
	if err != nil {
		fmt.Println("ListenAndServe error: ", err)
	}
}
