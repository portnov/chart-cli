chart-cli README
================

This repo provides a `chart` command-line utility. `chart` is aimed to generate
basic 2D charts from command line easily.


The `chart` utility is not aimed to give full control over generated charts, or
to generate all possible types of charts - you may anytime do that by use of
[gnuplot][1] or [Chart package][2]. This utility, instead, gives you a very
quick way of generating some most useful types of charts.

Usage example:

```
$ cat > input.txt
X	Y1	Y2
1	1	3
2	4	6
3	7	4
4	5	8
6	3	2
^D

$ chart --header -o lines.png input.txt
```

Result:

![lines.png](https://user-images.githubusercontent.com/284644/63638172-1f134600-c69e-11e9-98ce-4c274ca423f3.png)

Supported chart types for now are:

* Line charts (the default one)
* Bar charts (clustered and stacked)
* Area charts
* Point charts

Other chart types may be added later.

Expected input format
---------------------

Input data are expected to be provided as a series of lines; each line may
represent either one point to plot, or several points at the same X coordinate.

The first line may optionally represent names of the columns.

Other lines should consist of several numbers. Numbers are separated with a
delimiter. Default delimiter is TAB characater; you may specify another with
`-d` command line option.

If only one number per line is provided, then these numbers will be used as Y
values; for X values, line numbers (starting with 1) will be used.

If more than one number per line is provided, then for most chart types (except
for bar charts) the first column will be used as X values, and other will be
used as Y values.

There are plans to support date/time values as X values, but it is not implemented yet.

[1]: http://www.gnuplot.info/
[2]: http://hackage.haskell.org/package/Chart

