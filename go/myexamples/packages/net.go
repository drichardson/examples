package main

import (
    "log"
    "net"
    "os"
)

var udpServerAddress *net.UDPAddr 

func init() {
    log.Print("Initializing udpServerAddress in init")
    udpServerAddress = &net.UDPAddr{}
    udpServerAddress.IP = net.IPv4(127,0,0,1)
    udpServerAddress.Port = 5432
}

func main() {

    // Lookup DNS names
    log.SetFlags(log.Lmicroseconds)
    log.Print("LookupHost www.google.com...")
    addrs, err := net.LookupHost("www.google.com")
    if err != nil {
        log.Fatal("Error looking up www.google.com. ", err)
    }
    log.Print("Found addresses ", addrs)

    log.Print("LookupIP www.google.com...")
    ipaddrs, err := net.LookupIP("www.google.com")
    if err != nil {
        log.Fatal("Error looking up www.google.com. ", err)
    }
    log.Print("Found addresses ", ipaddrs)

    ip := net.ParseIP("4.2.2.1")
    log.Print("Parsed IP address: ", ip)

    mac, err := net.ParseMAC("12:34:aa:bb:cc:dd")
    if err != nil {
        log.Fatal("Couldn't parse mac addres. ", err)
    }
    log.Print("Parsed MAC address: ", mac)

    c := make(chan int)
    go udpserver(c)
    go udpclient(c)

    <-c
    <-c

    log.Print("Done")
}

func udpserver(c chan int) {
    serverLog := log.New(os.Stdout, "SERVER ", log.Flags())
    serverLog.Print("UDP Server starting...")

    connection, err := net.ListenUDP("udp", udpServerAddress)

    if err != nil {
        serverLog.Fatal("Error excepting connection. ", err)
    }

    defer func() { serverLog.Print("Closing connection in defer."); connection.Close(); }()

    var clientMsg [1024]byte
    n, clientAddress, err := connection.ReadFromUDP(clientMsg[:])
    if err != nil {
        serverLog.Fatal("Error reading message from client. ", err)
    }
    serverLog.Print("Read message ", clientMsg[0:n], " (", string(clientMsg[0:n]), ") from client ", clientAddress)

    //data := []byte{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20}
    data := []byte(`this is also a test`)
    n, err = connection.WriteTo(data, clientAddress)
    if err != nil {
        serverLog.Fatal("Error writing data. ", err)
    }

    serverLog.Print("Write returned n = ", n)

    c <- 1
}

func udpclient(c chan int) {
    clientLog := log.New(os.Stdout, "CLIENT ", log.Flags())
    clientLog.Print("UDP Client starting...")
    connection, err := net.DialUDP("udp", nil, udpServerAddress)

    if err != nil {
        clientLog.Fatal("DialUDP failed. ", err)
    }

    defer func() { clientLog.Print("Closing connection in defer."); connection.Close(); }()

    n, err := connection.Write([]byte(`hi earth`))

    if err != nil {
        clientLog.Fatal("WriteToUDP failed. ", err)
    }

    clientLog.Print("Wrote ", n, " bytes to server")

    serverMsg := make([]byte, 1024)
    n, _, err = connection.ReadFromUDP(serverMsg)
    if err != nil {
        clientLog.Fatal("Error reading message from server. ", err)
    }

    clientLog.Print("Read message ", serverMsg[0:n], " (", string(serverMsg[0:n]), ") from server.")

    c <- 1
}

