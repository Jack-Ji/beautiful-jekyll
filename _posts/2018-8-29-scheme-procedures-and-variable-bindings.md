---
layout: post
title: Scheme语言-Chapter 4-函数与变量绑定
bigimg: /img/John-McCarthy.jpg
tags: [scheme]
---

函数和变量绑定是`scheme`程序的重要组成部分，本章重点讲述与其相关的语法表达式。
我们先从最基础的变量引用和lambda定义开始，然后再讨论变量绑定和赋值（`define`、`letrec`、`let-values`、`set!`）。

其它也会绑定变量或者对变量进行赋值但主要目的不在于此的语法表达式（如命名`let`）会在第五章中讨论。

## 变量引用

```
语法：variable
返回：variable的值
```

凡是出现在绑定语句有效范围内的被绑定标识符都被`scheme`认为引用了某个变量，这样的绑定语句有`define`、`lambda`、`let`等。

```scheme
list => #<procedure>
(define x 'a)
(list x x) => (a a)
(let ([x 'b])
  (list x x)) => (b b)
(let ([let 'let]) let) => let
```

任何未被绑定的标识符都不能出现在库或者程序顶层中，否则`scheme`会报语法错误。
变量引用的定义语句不一定要出现在变量使用语句之前，只需保证变量引用在被正式求值之前定义已经结束即可。举例来讲：

```scheme
(define f
  (lambda (x)
    (g x)))
(define g
  (lambda (x)
    (+ x x)))
```

函数`f`使用了之后定义的函数`g`，由于函数f在没被执行前不会对`g`求值，以上代码不会有任何问题。
反之，下面代码中的函数`g`在定义结束之前被定义语句`define`调用了，因此`scheme`会报错。

```scheme
(define q (g 3))
(define g
  (lambda (x)
    (+ x x)))
```

## lambda

```
语法：(lambda formals body1 body2 ...)
返回：子程序
库：(rnrs base), (rnrs)
```

`lambda`表达式的作用是创建子程序。
`scheme`中任何涉及到创建子程序或局部变量的操作最终都通过某种形式的`lambda`或`case-lambda`语句完成。

`formals`字句表示子程序的形参，`body1 body2 ...`表示子程序的函数体。

可以在函数体的起始位置添加多个定义语句，其有效范围被局限在函数体内部。
当函数体的起始处出现定义语句时，函数体被扩展为`letrec*`表达式，局部定义通过其绑定语句完成，执行部分是其它函数执行语句。
如此一来，函数体中的定义表达式就被清理掉了。

创建子程序时，函数体中使用到的形参以外的所有外部变量都会被作为子程序的一部分保留下来。
当子程序被调用时，函数体中的形参被绑定至实参，其它的变量则会被恢复至原先的绑定，最后再对函数体进行求值。

子程序的实参被绑定至形参的具体过程如下：

* 当`formals`为`list`时（如`(x y z)`），实参和形参一一对应。如果实参和形参的数目不一致，`scheme`会抛出异常`&assertion`。
* 当`formals`为单个变量时（如`z`），该变量被绑定至所有实参组成的`list`。
* 当`formals`为结尾非空的非法`list`时（如`(x y . z)`），最后一个形参除外，其它形参和实参一一对应，最后一个形参被绑定至剩下的其它实参组成的`list`。
当实参的数目过少时，`scheme`会抛出异常`&assertion`。

在对函数体求值时，`scheme`会依次对函数体的每条表达式进行求值计算，最后一个表达式的求值结果被作为函数的结果返回。

函数或者子程序没有统一的打印输出格式，本书使用`#<procedure>`作为输出格式。

```scheme
(lambda (x) (+ x 3)) => #<procedure>
((lambda (x) (+ x 3) 7) => 10
((lambda (x y) (* x (+ x y))) 7 13) => 140
((lambda (f x) (f x x)) + 11) => 22
((lambda () (+ 3 4))) => 7

((lambda (x . y) (list x y))
  28 37) => (28 (37))
((lambda (x . y) (list x y))
  28 37 47 28) => (28 (37 47 28))
((lambda (x y . z) (list x y z))
  1 2 3 4) => (1 2 (3 4))
((lambda x x) 7 13) => (7 13)
```

## case-lambda

如前所述，`lambda`表达式创建的子程序只支持两种参数风格：固定长度的参数列表和大于等于某个长度的参数列表。

```scheme
(lambda (var1 ... varn) body1 body2 ...)
```

以上形式的`lambda`表达式只接受`n`个参数。

```scheme
(lambda (var1 ... varn . r) body1 body2 ...)
```

以上形式的`lambda`表达式可接受大于等于`n`个参数。

由此可见，`lambda`表达式灵活性相对有限（仅就参数列表这一点来讲），现代编程语言中常出现的参数默认值也没有被直接支持。
显而易见，通过对以上第二种形式的`lambda`表达式的入参进行分析，结合参数列表的长度检查和`car`、`cdr`函数，我们还是能够
实现参数默认值的，代价是更为晦涩的代码和性能的降低。

`case-lambda`语法表达式就是为针对这种情况而引入的语法扩展。

```
语法：(case-lambda clause ...)
返回：子程序
库：(rnrs control), (rnrs)
```

`case-lambda`由多个`cluase`组成，每个`clause`的定义形式如下：

```scheme
[formals body1 body2 ...]
```

上面的`formals`部分的含义和`lambda`表达式中的形参部分一模一样，它表示了当前函数所能接受的其中一种参数风格，多个`clause`组合起来
为函数提供了多种风格的参数列表和对应的函数体。

当`case-lambda`定义的函数被调用时，`scheme`会对所有的`clause`进行逐个分析，当遇到第一个能够接受当前参数的`clause`时，该`clause`的
`formals`中的所有形参被绑定至对应的实参，然后再对对应的函数体进行求值。如果没有任何`clause`能够和实参匹配成功，`scheme`会抛出异常
`&assertion`。

下面的函数`make-list`就是用`case-lambda`定义的，调用方不输入初始值的情况下，`make-list`默认将`#f`作为初始值：

```scheme
(define make-list
  (case-lambda
    [(n) (make-list n #f)]
    [(n x)
      (do ([n n (- n 1)] [ls '() (cons x ls)])
        ((zero? n) ls))]))
```

函数`substring`可通过`case-lambda`将`end`参数变为可选项，默认不输入表示字符串的结尾，
也可以`start`和`end`参数都不输入，这时`substring`的功能和`string-copy`一模一样：

```scheme
(define substring1
  (case-lambda
    [(s) (substring1 s 0 (string-length s))]
    [(s start) (substring1 s start (string-layout s))]
    [(s start end) (substring s start end)]))
```

还可以在只有一个参数时将`start`默认为字符串起始位置：

```scheme
(define substring2
  (case-lambda
    [(s) (substring2 s 0 (string-length s))]
    [(s end) (substring2 s 0 end)]
    [(s start end) (substring s start end)]))
```

我们甚至可以让`start`和`end`参数要么同时出现，要么都使用默认值：

```scheme
(define substring3
  (case-lambda
    [(s) (substring3 s 0 (string-length s))]
    [(s start end) (substring s start end)]))
```

## 本地绑定

```
语法：(let ((var expr) ...) body1 body2 ...)
返回：绑定语句块的最后一个表达式的求值结果
库：(rnrs base), (rnrs)
```

`let`表达式被用于创建本地绑定，绑定语句中的每个`var`标识符均被绑定至对应的`expr`的求值结果。
`let`的语句部分（body1 body2 ...）的求值方式跟`lambda`的函数体部分一样。

`let`还有其它几种功能相近的“近亲”表达式：`let*`、`letrec`和`letrec*`。
`let`与`let*`、`letrec`、`letrec*`的不同之处在于绑定语句的求值表达式`expr`无法使用任何被绑定的变量`var`。
`let`与`let*`、`letrec*`的不同之处在于每个`var`的绑定顺序是不可预知。

```scheme
(let ([x (* 3.0 3.0)] [y (* 4.0 4.0)])
  (sqrt (+ x y))) => 5.0

(let ([x 'a] [y '(b c)])
  (cons x y)) => (a b c)

(let ([x 0] [y 1])
  (let ([x y] [y x])
    (list x y))) => (1 0)
```

下面的代码利用`lambda`将`let`表达式定义为语法扩展：

```scheme
(define-syntax let
  (syntax-rules ()
    [(_ ((x e) ...) b1 b2 ...)
      ((lambda (x ...) b1 b2 ...) e ...)]))
```

带命名的`let`表达式将会在`5.4`节中再描述。

```
语法：(let* ((var expr) ...) body1 body2 ...)
返回：绑定语句块的最后一个表达式的求值结果
库：(rnrs base), (rnrs)
```

`let*`的作用与`let`类似，区别在与`let*`的绑定语句的求值顺序是固定的从左至右。
因此，当绑定语句的求值顺序对最终的求值结果有影响时，可以使用`let*`保证绑定语句的执行顺序。

```scheme
(let* ([x (* 5.0 5.0)]
       [y (- x (* 4.0 4.0))])
  (sqrt y)) => 3.0

(let ([x 0] [y 1])
  (let* ([x y] [y x])
    (list x y))) => (1 1)
```

任何`let*`表达式均可被转换为多层嵌套的`let`表达式，具体如下：


```scheme
(define-syntax let*
  (syntax-rules ()
    [(_ () e1 e2 ...)
      (let () e1 e2 ...)]
    [(_ ((x1 v1) (x2 v2) ...) e1 e2 ...)
      (let ((x1 v1))
        (let* ((x2 v2) ...) e1 e2 ...))]))
```

```
语法：(letrec ((var expr) ...) body1 body2 ...)
返回：绑定语句块的最后一个表达式的求值结果
库：(rnrs base), (rnrs)
```

`letrec`的作用和`let`、`let*`类似，区别是`letrec`的绑定语句的表达式可任意访问其它绑定变量`var`。
因此`letrec`可被用于定义多个互相调用的递归函数。

```scheme
(letrec ([sum (lambda (x)
                (if (zero? x)
                    0
                    (+ x (sum (- x 1)))))])
  (sum 5)) => 15
```

由于`letrec`的绑定语句的执行顺序是不确定的，在绑定语句块结束前不能对绑定变量进行求值，否则`scheme`会抛出`&assertion`异常。
（注意，定义`lambda`语句时不会对其中出现的外部变量马上进行求值，而是等到之后函数被调用时才会进行求值）。

当绑定变量之间有依赖，并且其求值顺序无所谓时，优先选择`letrec`表达式。
当绑定变量之间有依赖，并且其求值顺序需要从左至右依次执行时，优先选择`letrec*`表达式。

形如`(letrec ((var expr) ...) body1 body2 ...)`的`letrec`表达式可通过`let`和`set!`来定义：

```scheme
(let ((var #f) ...)
  (let ((temp expr) ...)
    (set! var temp)
    ...
    (let ()
      body1 body2 ...)))
```

上面的`temp ...`是一组新的变量，每个`var`变量都存在一个对应的`temp`变量。
最外层的`let`语句首先创建所有的`var`变量，初始值在这时并不重要，因此全部初始为`#f`。
这样做的目的是为了保证变量`var ...`可以在后面的`expr ...`语句中出现。
中间的`let`语句对`expr ...`进行求值，并且将求值结果绑定至每个对应的`temp`变量。
最内层又加了一个`let`语句是为了应对`body1 body2 ...`包含内部定义的情况（还记得吗，内部定义只能出现在语句块的首部）。

```
语法：(letrec* ((var expr) ...) body1 body2 ...)
返回：绑定语句块的最后一个表达式的求值结果
库：(rnrs base), (rnrs)
```

`letrec*`的作用和`letrec`同样类似，区别是`letrec*`对`expr ...`的求值严格按照从左至右的顺序进行。
因此，`letrec*`中后面的绑定语句可以正常对前面出现的绑定变量进行求值计算。

形如`(letrec* ((var expr) ...) body1 body2 ...)`的`letrec`表达式同样可通过`let`和`set!`来定义：


```scheme
(let ((var #f) ...)
  (set! var expr) ...
  (let () body1 body2 ...))
```

最外层的`let`表达式创建了`var ...`变量，然后依次对每个`expr`进行求值后再赋给对应的`var`变量。
最后，使用`let`而不是`begin`语句来封装`body1 body2 ...`的原因同样是为了应对`body1 body2 ...`包含内部定义的情况。


```scheme
(letrec* ([sum (lambda (x)
                 (if (zero? x)
                     0
                     (+ x (sum (- x 1)))))]
          [f (lambda () (cons n n-sum))]
          [n 15]
          [n-sum (sum n)])
  (f)) => (15 . 120)

(letrec* ([f (lambda () (lambda () g))]
          [g (f)])
  (eq? (g) g)) => #t

(letrec* ([g (f)]
          [f (lambda () (lambda () g))])
  (eq? (g) g)) => exception: attempt to reference undefined variable f
```

## 多变量绑定

