package main

import (
    "log"
    "net/http"
    "time"
    "os"
    "fmt"
    "io/ioutil"
    _ "expvar"
)

func newLogger(prefix string) (l *log.Logger) {
    l = log.New(os.Stdout, prefix + " ", log.Lmicroseconds | log.Ldate)
    return
}

func main() {
    go httpServer8080()
    go httpServer7070()
    go httpServer6060()

    // Wait for HTTP server to start
    time.Sleep(500*time.Microsecond)

    httpClient(8080)
    httpClient(7070)
    httpClient(6060)

    c := make(chan int)
    <-c
}


// 8080 server uses a custom handler
func httpServer8080() {
    l := newLogger("SERVER-8080")

    handler := newCustomHandler(l)

    server := &http.Server{
        Addr: ":8080",
        Handler: handler,
    }

    l.Print("Listening on ", server.Addr)
    err := server.ListenAndServe()
    l.Fatal("Stopped. ", err)
}

type customHandler struct {
    logger *log.Logger
}

func newCustomHandler(l* log.Logger) (result *customHandler) {
    result = new(customHandler)
    result.logger = l
    return
}

func (h *customHandler)Log(args...interface{}) {
    h.logger.Print(args...)
}

func (h *customHandler)ServeHTTP(w http.ResponseWriter, req *http.Request) {
    h.Log("Serving request for ", req.URL.Path) 

    switch req.URL.Path {
    case "/time":
        fmt.Fprint(w, "8080 Current server time is ", time.Now())
    case "/hostname":
        name, err := os.Hostname()
        if err == nil {
            fmt.Fprint(w, "8080 Hostname is ", name)
        } else {
            w.WriteHeader(http.StatusInternalServerError)
        }
    default:
        w.WriteHeader(http.StatusNotFound)
    }
}

// 7070 server uses built-in everything
func httpServer7070() {
    l := newLogger("SERVER-7070")

    http.HandleFunc("/time", func(w http.ResponseWriter, r *http.Request) {
        fmt.Fprint(w, "7070 Current server time is ", time.Now())
    })

    http.HandleFunc("/hostname", func(w http.ResponseWriter, r *http.Request) {
        name, err := os.Hostname()
        if err == nil {
            fmt.Fprint(w, "7070 Hostname is ", name)
        } else {
            w.WriteHeader(http.StatusInternalServerError)
        }
    })

    l.Print("Listening")
    err := http.ListenAndServe(":7070", nil)
    l.Fatal("Error ListenAndServer on 7070. ", err)
}


// 6060 server uses ServerMux handler
func httpServer6060() {
    l := newLogger("SERVER-6060")

    mux := http.NewServeMux()
    mux.HandleFunc("/time", func(w http.ResponseWriter, r *http.Request) {
        fmt.Fprint(w, "6060 Current server time is ", time.Now())
    })
    mux.HandleFunc("/hostname", func(w http.ResponseWriter, r *http.Request) {
        name, err := os.Hostname()
        if err != nil {
            w.WriteHeader(http.StatusInternalServerError)
        } else {
            fmt.Fprint(w, "6060 Hostname is ", name)
        }
    })

    server := &http.Server{
        Addr: ":6060",
        Handler: mux,
    }

    l.Print("Listening")
    err := server.ListenAndServe()
    l.Fatal("Failed ", err)
}


func httpClient(port int) {
    l := newLogger(fmt.Sprint("CLIENT-", port))
    baseaddr := fmt.Sprint("http://localhost:", port)

    for _, urlstr := range []string{"unknown", "time", "hostname"} {
        addr := baseaddr + "/" + urlstr
        l.Print("Connecting to ", addr)
        resp, err := http.Get(addr)
        if err != nil {
            l.Fatal("Error connecting to ", addr, ". Error: ", err)
        }
        defer resp.Body.Close()

        body, err := ioutil.ReadAll(resp.Body)
        if err != nil {
            l.Print("Error reading body. ", err)
        } else {
            l.Print("Response received. ", resp.Status, " body: ", string(body))
        }
    }
}

