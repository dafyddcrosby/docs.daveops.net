---
title: Go
---

## Syntax cheatsheet

```go
package main

import (
	"fmt"
	"os"
	"bufio"
)

func read_file(path string) string {
        f, err := os.Open(filearg)
        if err != nil {
                panic(err)
        }
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}
}

func main() {
        fmt.Println("hello world")
}
```

## Go modules

```bash
go mod init <module_name>
go mod vendor
go get -u
```

## Cross-compilation

```bash
# Compile for AMD64 Linux
GOOS=linux GOARCH=amd64 go build
```

[https://golang.org/doc/install/source#environment](List of compilation targets)

## Static linking

If you're not using CGo, you can statically link with `CGO_ENABLED`.
In Fedora this appears to be [on by default](https://src.fedoraproject.org/rpms/golang//blob/rawhide/f/golang.spec)

```bash
CGO_ENABLED=0 go build
```

## Resources

```bash
godoc -http=:8080
```

- <https://golang.org/>
- [Language Specification](https://golang.org/ref/spec)
- [Go Koans](https://github.com/cdarwin/go-koans)

## Looking into

- <https://www.kablamo.com.au/blog-1/2018/12/10/just-tell-me-how-to-use-go-modules>
- <https://cryptic.io/go-http/>
- <https://medium.com/@nate510/don-t-use-go-s-default-http-client-4804cb19f779>
- [TinyGo](https://tinygo.org/)

## Templating

```
{{/* comment */}}  Defines a comment
{{.}}              Renders root element
{{.Foo}}           Renders the "Foo"-field in a nested element

{{if .Done}} {{else}} {{end}}   Defines an if-statement
{{range .Items}} {{.}} {{end}}  Loops over all “Items” and renders each using {{.}}
{{block "bar" .}} {{end}}       Defines a block with the name "bar"
```

# Go - FastCGI

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


# Mage

https://magefile.org/
