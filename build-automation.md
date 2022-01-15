# Build automation

# autoconf

## Check library for function

```
AC_CHECK_LIB (library, function, [action-if-found], [action-if-not-found], [other-libraries])
```





# Makefile

```makefile
name= project_name
version= 3.3

all: 
	@true

clean:
	$(RM) -rf *.o *~ *.rpm *.spec *.tar.gz

test:
	pylint -E *.py
	python -m doctest -v *.py

install:
	mkdir -p /usr/bin
	install $(bin_files) /usr/bin/

# Make a spec file out of a spec.in file
%.spec: %.spec.in
	sed -e 's/@VERSION@/$(version)/' $(name).spec.in > $(name).spec

dist: clean $(name).spec
	mkdir $(name)-$(version)
	cp -r $(files) $(name)-$(version)
	tar zcf $(name)-$(version).tar.gz $(name)-$(version)
	rm -rf $(name)-$(version)
```


# qmake
## Generate Makefile



  qmake -o Makefile hello.pro



# Ninja (build system)

## Create compilation database

```
ninja -t compdb > compile_commands.json
```

## Links

- https://ninja-build.org/
# watchman

- https://facebook.github.io/watchman/
