# North
**A concatenative, pattern based programming language**

## Examples

Pattern-matching
```rb
def fib ( 0 ) 0 end
def fib ( 1 ) 1 end
def fib ( n ) ((n 1 -) fib) ((n 2 -) fib) + end
```

Concatenative
```rb
def dup ( x ) x x end
def square ( x ) x dup * end
```