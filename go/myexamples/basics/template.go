package main

import (
	"text/template"
	"os"
	"fmt"
)

type MyType string;

func (s *MyType) Key1() string {
	return "MyType";
}

func main() {

	var t *template.Template = template.Must(template.New("name").Parse(templateStr))

    if err := t.Execute(os.Stdout, map[string]string{"Key1": "value1"}); err != nil {
		fmt.Printf("Error executing using map. %v", err)
		return
	}

	var myType MyType = "mytype value 1"

	if err := t.Execute(os.Stdout, &myType); err != nil {
		fmt.Printf("Error executing using custom type. %v", err)
		return
	}
}

const templateStr = `
--------------
This is a test
--------------
key1: {{.Key1}}
`
