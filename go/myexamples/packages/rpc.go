package main

import (
    "net/rpc"
    "log"
    "os"
    "net"
    "net/http"
    "time"
)

type Accumulator int

type RemoteSumError string
func (r* RemoteSumError) Error() string {
    return "RemoteSumError: " + string(*r)
}

func (t *Accumulator) RemoteSum(arguments []int, reply *int) error {
    // Reply is the old value
    *reply = int(*t)

    for _, value := range arguments {
        *t += Accumulator(value)
    }

    *reply = int(*t)

    //result := new(RemoteSumError)
    //*result = `this is doug's error`
    //return result
    return nil
}



func main() {

    log.SetFlags(log.Ldate|log.Lmicroseconds)

    go server()

    // Wait for server to start up
    time.Sleep(500*time.Millisecond)
    client()
}

func server() {
    slog := log.New(os.Stdout, "SERVER ", log.Flags())
    slog.Print("starting")

    a := new(Accumulator)
    rpc.Register(a)
    rpc.HandleHTTP()
    l, e := net.Listen("tcp", ":1234")
    if e != nil {
        slog.Fatal("Error listening. ", e)
    }

    http.Serve(l, nil)
}

func client() {
    clog := log.New(os.Stdout, "CLIENT ", log.Flags())
    clog.Print("starting")

    client, err := rpc.DialHTTP("tcp", "127.0.0.1:1234")
    if err != nil {
        clog.Fatal("DialHTTP failed. ", err)
    }

    clog.Print("Calling RemoteSum")


    var reply int
    err = client.Call("Accumulator.RemoteSum", []int{1,2,3}, &reply)
    if err != nil {
        clog.Fatal("Error calling RemoteSum ", err)
    }

    clog.Print("RemoteSum return ", reply)
}

