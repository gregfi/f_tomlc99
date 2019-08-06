# f_tomlc99
Fortran interface to [TOML](https://github.com/toml-lang/toml) through cktan's [tomlc99](https://github.com/cktan/tomlc99) library. The interface hasn't been tested extensively, so please use caution. Suggestions and fixes are welcome. 

See the `example_program.f90` file for the available facilities. Note that the functions that retrieve key names for arrays and tables are currently missing. The example can be compiled with:

`$FC tomlc99.f90 example_program.f90 /path/to/tomlc99/libtoml.a`

The example code produces the following output on my machine:

```
first key string length: 5
first key string value: title

nkval=6; narr=7; ntab=0

'host' type=s; length=33; value=www.example.com_ǝnlɐʌ‾ǝɯos
'port' type=i; value=80
'uptime' type=d; value=123.4
'enabled' type=b; value=T
'ldt1' type=T; value=1979-05-27T07:32:00-05:00
'키' type=s; length=1; raw_value=값; unicode_int=44050

'host' is ascii: T
'키' is ascii: F

name    kind  type #elem
intArray   v     i     5
dblArray   v     d     5
boolArray  v     b     5
dateArray  v     D     5
strArray   v     s     5     6
strArray:   str1    str2    str3    str4    str__5

intArray:    1   2   3   4   5
dblArray:    1.0   2.0   3.0   4.0   5.0
boolArray:  T F T F T
dateArray: 2019-01-01 2019-01-02 2019-01-03 2019-01-04 2019-01-05 
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
