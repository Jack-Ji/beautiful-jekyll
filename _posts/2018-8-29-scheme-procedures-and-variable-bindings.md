---
layout: post
title: Scheme语言-Chapter 4-函数与变量绑定
bigimg: /img/John-McCarthy.jpg
tags: [scheme]
---

函数和变量绑定是scheme程序的重要组成部分，本章重点讲述与其相关的语法表达式。
我们先从最基础的变量引用和lambda定义开始，然后再讨论变量绑定和赋值（define、letrec、let-values、set!）。

其它也会绑定变量或者对变量进行赋值但主要目的不在于此的语法表达式（如命名let）会在第五章中讨论。

## 变量引用

```
语法：variable
返回：variable的值
```

凡是出现在绑定语句有效范围内的被绑定标识符都被scheme认为引用了某个变量，这样的绑定语句有define、lambda、let等。

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

