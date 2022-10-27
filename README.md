# North
**A concatenative, functional, pattern based programming language**

## Examples
Concatenative
```rb
def dup ( x ) x x end
def square ( x ) x dup * end
def fib ( max )
    def f ( x y 1 ) y end
    def f ( x y i ) (y) (x y +) (i 1 -) f end
    0 1 max f
end

2 dup       # [ 2, 2 ]
9 square    # [ 81 ]
5 fib       # [ 55 ]
```

Pattern based
```rb
def fib ( 0 ) 0 end
def fib ( 1 ) 1 end
def fib ( n ) ((n 1 -) fib) ((n 2 -) fib) + end

5 fib   # [ 55 ]
```

```rb
def applyn ( x f 0 ) x end
def applyn ( x f n )
    (x f) &f (n 1 -) applyn
end

def double ( x ) x 2 * end

# apply double five times to '2'
2 &double 5 applyn

# [ 64 ]
```