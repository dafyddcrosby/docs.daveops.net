# Graphing
# gnuplot

http://www.gnuplot.info/

```
plot "filename" with lines title "This is a file"
set xlabel "foo"
set ylabel "bar"
```


## Use CSV files

```
set datafile separator ","
```
# Graphviz

<http://graphviz.org>

## Output SVG

```bash
dot file.dot -Tsvg -o output.svg
```

## Plain graph

```
digraph graph_name {
  A [label="Node A"] // sample comment
  B [label="Node B"]
  A->B [label="commands", fontcolor=red]
  X->Y->Z
}
```

## Common attributes

name         | desc
---          | ---
bgcolor      | background color
tooltip      | mouseover functionality for SVG
URL          | clickable link
label        | node content
labelURL     |
image        | sets image as node content - JPEG, PNG, GIF, BMP, SVG, or Postscript, image must contain size info
shape        | shape of the node
