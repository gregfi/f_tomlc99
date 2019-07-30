# f_tomlc99
Fortran interface for cktan's [tomlc99](https://github.com/cktan/tomlc99) library. The interface seems to work, but hasn't been tested extensively, so please use caution. Suggestions and fixes are welcome. 

See the `example_program.f90` file for available facilities. Note that timestamp and utf8 support have not yet been implemented. The example can be compiled with:

`$FC tomlc99.f90 example_program.f90 /path/to/libtoml.a`

The example code produces the following output on my machine:

```
first key string length: 5
first key string value: title

'host' type=s; length=15; value=www.example.com
'port' type=i; value=80
'uptime' type=d; value=123.4
'enabled' type=b; value=T

name    kind  type #elem
intArray   v     i     5
dblArray   v     d     5
boolArray  v     b     5
strArray   v     s     5

inArray:    1   2   3   4   5
dblArray:    1.0   2.0   3.0   4.0   5.0
boolArray:  T F T F T
strArray:   str1    str2    str3    str4    str__5

'port' kind: v
'boolArray' kind: a
'server' kind: t
'notpresent' kind: (c_null_char)

points: array kind=t; #elem=3
       1.0   2.0   3.0
       4.0   5.0   6.0
       7.0   8.0   9.0

alt_points: array kind=a; #elem=3
       1.0   2.0   3.0
       4.0   5.0   6.0
       7.0   8.0   9.0
```
