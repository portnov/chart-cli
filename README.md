chart-cli README
================

[![Build Status](https://travis-ci.org/portnov/chart-cli.svg?branch=master)](https://travis-ci.org/portnov/chart-cli)

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

Supported output formats are:
  
* PNG (the default one)
* SVG
* PS (PostScript)
* PDF

Output file format is automatically detected by specified output file name. If
output file name is not specified explicitly, `chart.png` will be used.

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

In the first column, date/time values may be provided instead of numbers (to be
used as values along X axis). Supported date/time formats are:

* DD.MM.YYYY
* YYYY/MM/DD
* `12 September 2012'
* today, tomorrow, yesterday
* `in 2 days', '3 weeks ago'
* `last monday', 'next friday'
* `last month' (1th of this month), `next year' (1th of January of next year)

(thanks to [dates package][3]).

Command-line interface
----------------------

I'll put it here for quick reference; more actual information is always
accessible with `chart --help`:

```
Usage: chart [-o|--output OUTPUT.png] [COMMAND] [-1|--header]
             [-d|--delimiter CHAR] [-i|--index] [-t|--title TITLE]
             [-w|--width WIDTH] [-h|--height HEIGHT] [-b|--background COLOR]
             [-f|--foreground COLOR] [-L|--legend ON|OFF] [INPUT.txt]
  Make a chart

Available options:
  -o,--output OUTPUT.png   write output to OUTPUT.png
  -1,--header              first line contains column headers
  -d,--delimiter CHAR      specify fields delimiter ('\t' by default)
  -i,--index               if enabled, treat input data as if there was an
                           additional first column, containing line numbers,
                           starting from 1
  -t,--title TITLE         set chart title to TITLE
  -w,--width WIDTH         specify chart width, in pixels (default: 800)
  -h,--height HEIGHT       specify chart height, in pixels (default: 600)
  -b,--background COLOR    specify background color name (see SVG 1.1 spec)
  -f,--foreground COLOR    specify foreround color name (see SVG 1.1 spec)
  -L,--legend ON|OFF       enable or disable the legend (default: True)
  -h,--help                Show this help text

Available commands:
  line                     Make a line chart
  area                     Make an area chart
  points                   Make a points chart
  bar                      Make a bar chart
```

Installation
------------

Install it by stack:

```
$ sudo apt-get install stack
$ git clone https://github.com/portnov/chart-cli.git
$ cd chart-cli/
$ stack install
```

[1]: http://www.gnuplot.info/
[2]: http://hackage.haskell.org/package/Chart
[3]: http://hackage.haskell.org/package/dates

