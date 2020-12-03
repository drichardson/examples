package main

import (
	"database/sql"
	"flag"
	"fmt"
	_ "github.com/mattn/go-sqlite3"
	"log"
	"os"
)

func main() {

	log.SetFlags(log.Lshortfile)

	var dbFilename = flag.String("db", "test.db", "database filename")
	flag.Parse()

	os.Remove(*dbFilename)

	db, err := sql.Open("sqlite3", *dbFilename)
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()

	_, err = db.Exec(`CREATE TABLE contact(name TEXT, email TEXT)`)
	if err != nil {
		log.Fatalf("Couldn't create table. %s")
	}

	tx, err := db.Begin()
	if err != nil {
		log.Fatal(err)
	}

	stmt, err := tx.Prepare(`INSERT INTO contact(name, email) values(?,?)`)
	if err != nil {
		log.Fatal(err)
	}
	defer stmt.Close()

	_, err = stmt.Exec("doug", "doug@hotmale.com")
	if err != nil {
		log.Fatal(err)
	}

	_, err = stmt.Exec("rebecca", "rebecca@gmail.com")
	if err != nil {
		log.Fatal(err)
	}

	tx.Commit()

	rows, err := db.Query("select name, email from contact")
	if err != nil {
		log.Fatal(err)
	}
	defer rows.Close()

	for rows.Next() {
		var name, email string
		rows.Scan(&name, &email)
		fmt.Println(name, email)
	}
}
