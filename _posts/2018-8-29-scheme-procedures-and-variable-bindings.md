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

```
语法：(case-lambda clause ...)
返回：子程序
库：(rnrs control), (rnrs)
```
