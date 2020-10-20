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

## Resources

```bash
godoc -http=:8080
```

* <https://golang.org/>
* [Language Specification](https://golang.org/ref/spec)
* [Go Koans](https://github.com/cdarwin/go-koans)

## Looking into

* https://www.kablamo.com.au/blog-1/2018/12/10/just-tell-me-how-to-use-go-modules
* https://cryptic.io/go-http/
* https://medium.com/@nate510/don-t-use-go-s-default-http-client-4804cb19f779
* [TinyGo](https://tinygo.org/)
