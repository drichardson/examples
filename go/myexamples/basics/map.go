package main

import "fmt"

func main() {
    var m1 = map[string] string {
        "k1" : "v1",
        "k2" : "v2",
        "k3" : "v3",
    };

    v4, ok := m1["k4"]

    fmt.Println(m1["k1"], m1["k2"], m1["k3"], ok, v4);

    var m2 = make(map[string] string);
    m2["ka"] = "va";
    m2["kb"] = "vb";
    fmt.Println(m2["ka"], m2["kb"]); 

}



