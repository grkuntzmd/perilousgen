package main

import (
	"log"
	"net/http"
	"strings"
)

func main() {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		println(r.URL.Path)
		if r.URL.Path == "/perilousgen" {
			http.Redirect(w, r, "/perilousgen/index.html", http.StatusMovedPermanently)
			return
		}
		if strings.HasPrefix(r.URL.Path, "/perilousgen") {
			http.StripPrefix("/perilousgen", http.FileServer(http.Dir("./docs"))).ServeHTTP(w, r)
			return
		}
		http.Redirect(w, r, "/perilousgen/index.html", http.StatusMovedPermanently)
	})

	log.Fatal(http.ListenAndServe(":8080", nil))
}
