package main

import "fmt"
import "encoding/json"

func main() {
    // Generate some json
    boolJson, _ := json.Marshal(true)
    fmt.Printf("json bool: %v\n", string(boolJson));

    intJson, _ := json.Marshal(123);
    fmt.Printf("json int: %v\n", string(intJson));

    mapJson, _ := json.Marshal(map[string]interface{}{"intVal": 444, "floatVal": 3.14, "stringVal": "stringy", "arrayVal": []int{1, 2, 3}, "mapVal": map[string]interface{}{"a": 1, "b": false, "c": "hi"}})
    fmt.Printf("json map: %v\n", string(mapJson));


    // Parse some json
    jsonText := []byte(`{"key1": 123, "key2": [1,2,3], "key3": { "a": 1, "b": 2 }}`)
    //jsonText = []byte(`1`)
    var parsed interface{}
    error := json.Unmarshal(jsonText, &parsed)
    if error == nil {
        fmt.Printf("json parse succeess. Got type %T\n", parsed)
        switch m := parsed.(type) {
        case map[string]interface{}:
            fmt.Printf("Got expected map: %v\n", m)
        default:
            fmt.Printf("Some other type: %v\n", m)
        }
    } else {
        fmt.Printf("json parse error: %v\n", error)
    }
}
