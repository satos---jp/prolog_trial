# prolog_trial
swipl like prolog interpreter (for functional programming experiment in IS)
## implemented
- occurrence check
- list notation
- cut
- bfs (in branch bfs)

## usage
```
$ cat tes.pl
x(a).
x(b).

$ ./main.exe
?- ['tes.pl'].
load tes.pl
?- list.
x(a).
x(b).

?- x(X).
[ X ::= b ];
[ X ::= a ];
false.
?-
```
