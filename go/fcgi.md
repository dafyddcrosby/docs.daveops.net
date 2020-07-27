---
title: Go - FastCGI
---

```go
package main

import (
	"fmt"
	"net/http"
	"net/http/fcgi"
)

func hello(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintln(w, "Hello")
}

func main() {
	http.HandleFunc("/", hello)

	err := fcgi.Serve(nil, nil)
	if err != nil {
		panic(err)
	}
}
```
