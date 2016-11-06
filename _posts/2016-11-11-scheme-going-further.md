---
layout: post
title: Scheme语言-Chapter 3-再进一步
bigimg: /img/John-McCarthy.jpg
tags: [scheme]
---

上一章介绍了Scheme中最常用到的一些语法结构及函数，你应该已经能够使用它们进行Scheme编程工作了。
本章会进一步为你介绍Scheme的其它特性和编程技巧，你可以使用它们编写更为复杂和高效的程序。

## 语法扩展

在上一章中我们提到过，`let`表达式是基于`lambda`表达式所做的语法扩展，虽然它们俩都属于Scheme核心语法表达式。
本小节将进一步为你介绍更多关于语法扩展的知识。

大多数Scheme实现（无论是编译器还是解释器）都有自己的核心语法集，所有其它的语法结构都是基于核心语法集所做的语法扩展。
Scheme处理程序的第一步就是将各种语法扩展翻译成核心语法集，这样一来剩下的工作就全部集中到了对核心语法集的处理上。

> 注意：不同的Scheme实现的核心语法集不见得是一样的，本文所讲的核心语法集只是一个示例而已。

尽管核心语法集应该包含哪些内容是个争论焦点，有一条是必须满足的：其它的语法表达式必须能够被准确翻译至核心语法集。

我们即将使用的核心语法集包含：`define`表达式、常量、变量、子程序调用表达式、`quote`表达式、`lambda`表达式、`if`表达式和`set!`表达式。
下面的语法描述总结了该核心语法集：`|`用于连接多种可能，`*`表示之前的表达式出现任意次数、`<variable>`是任意合法的Scheme标识符，
`<datum>`表示任意Scheme数据对象（如数字、list、符号、向量等），`<boolean>`表示`#t`或`#f`，`<number>`表示任何数字，
`<character>`表示任何字符、`<string>`表示任何字符串。

```
<program>               --> <form>*
<form>                  --> <definition> | <expression>
<definition>            --> <variable definition> | (begin <definition>*)
<variable definition>   --> (define <variable> <expression>)
<expression>            --> <constant>
                         |  <variable>
                         |  (quote <datum>)
                         |  (lambda <formals> <expression> <expression>*)
                         |  (if <expression> <expression> <expression>)
                         |  (set! <variable> <expression>)
                         |  <application>
<constant>              --> <boolean> | <number> | <character> | <string>
<formals>               --> <variable>
                         |  (<variable>*)
                         |  (<variable> <variable>* . <variable>)
<application>           --> (<expression> <expression>*)
```

以上语法描述中的`<application>`（子程序调用）的定义比较容易和`quote`、`lambda`、`if`、`set!`混淆。
因此为了对它们作区分，`<application>`的第一个子项不允许是`quote`、`lambda`、`if`和`set!`这些关键字之一，
除非这些关键字被重新定义了。

上一章介绍的简化的函数定义语法没有被包含到核心语法集中，这是因为该语法可以很容易的被翻译为`define`语句。
类似的，核心`if`表达式不允许`else`子项被忽略不写，
因为忽略了`else`子项的`if`语句也可以很容易的被翻译为核心`if`表达式——将`else`子项写成常量（如`#f`）即可。

包含多个`define`语句的`begin`语句同样被认为是定义语句，这是为了允许语法扩展被翻译为包含多个定义的语句。
包含多个表达式的`begin`语句不属于核心语法，因为它可以很容易的被翻译为`lambda`调用语句：

```
(begin e1 e2 ...) --> ((lambda () e1 e2 ...))
```

确立了核心语法集后，我们就可以讨论语法扩展了。
语法扩展，顾名思义，它使我们能够扩展Scheme的语法，使其语法结构不局限于核心语法集。
语法扩展不止可以基于核心语法集来编写，同样也可以基于其它语法扩展来编写，当然它们最终都会被Scheme转换为核心语法集。

定义语法扩展的关键字是`define-syntax`，它会将一个语法翻译过程与指定的关键字相关联。下面给出了`let`语句的定义：

```scheme
(define-syntax let
    (syntax-rules ()
        [(_ ((x e) ...) b1 b2 ...)
         ((lambda (x ...) b1 b2 ...) e ...)]))
```

我们称`define-syntax`后面的标识符为`语法扩展的关键字`，也就是本例中的`let`。
`syntax-rules`表达式会生成语法翻译过程，`syntax-rules`的后面是用于填写`辅助关键字`的list。
辅助关键字很少用到，一个例子是`cond`语句中的`else`，我们将会在第8章讨论更多使用辅助关键字的语法扩展。
紧跟着辅助关键字的是一条或多条翻译规则，也称为`模式/模板pair`。上面的`let`只有一条翻译规则或模式/模板pair。
翻译规则中的`模式`部分负责描述程序语句的结构，`模板`部分则给出了翻译后的结果。

`模式`使用的括号表达式的第一个元素总是`_`（在第8章我们将知道这里使用下划线其实是个预定成俗的惯例）。
如果有多条翻译规则，Scheme会根据输入内容匹配合适的`模式`以确定使用哪个`模板`进行翻译。
如果没有任何`模式`能与输入匹配得上，Scheme会抛出语法错误异常。

`模式`中除了下划线、省略号和辅助关键字以外的标识符被称为`模式变量`。
`模式变量`能够匹配输入中对应的任何类型的子项，它在`模板`中被代表所匹配的内容。
`模式`中的`“pat ...”`表示0个或多个与`pat`匹配的表达式。同样的，`模板`中的`“expr ...”`代表了0个或多个与其相匹配的输入。
输入中的`pat`的数目决定了语法扩展输出的`expr`的数目。

以上`let`语法扩展的翻译规则大体上很容易理解，除了以下几点需要特别说明一下：

1. 由于`let`语法要求其正文部分至少有一个表达式，我们采用了写法`“b1 b2 ...”`，而不是`“b ...”`；
2. 尽管变量`x`和`e`在模式中共享同一个省略号，它们仍然可以在模板中被分开使用；
2. **使用了省略号的模式变量（`x`、`e`和`b2`）在模板中被使用时必须也要加上省略号。**

我们再看一个稍复杂点的语法扩展。下面是`and`语句的定义：

```scheme
(define-syntax and
    (syntax-rules ()
        [(_) #t]
        [(_ e) e]
        [(_ e1 e2 e3 ...)
            (if e1 (and e2 e3 ...) #f)]))
```

很显然，`and`有多条翻译规则，并且采用了递归调用。
我们在上一章中讲过，`(and)`的值为`#t`，`and`的第一条翻译规则就对这种情况进行了处理。
第2条和第3条翻译规则分别是`基准情形`和`递归步进`，它们将有多个参数的`and`表达式转换为了互相嵌套的`if`表达式。

```
(and a b c)
--> (if a (and b c) #f)
--> (if a (if b (and c) #f) #f)
--> (if a (if b c #f) #f)
```

通过观察展开后的表达式可得知，若`a`和`b`的值为真，则求值结果为`c`的值，否则结果为`#f`。
这和我们在上一章介绍的`and`语句的行为是一致的。

下面是另一个`and`实现，尽管看起来要更简洁些，很遗憾它是不正确的。

```scheme
(define-syntax and
    (syntax-rules ()
        [(_) #t]
        [(_ e1 e2 ...)
            (if e1 (and e2 ...) #f)]))
```

让我们按照以上定义分析表达式`(and (not (= x 0)) (/ 1 x))`的翻译过程。
假设`x`的值不为0，表达式的求值结果应该是`1/x`。而按照以上定义，表达式的展开形式如下：

```
(if (not (= x 0) (and (/ 1 x)) #f)
--> (if (not (= x 0)) (if (/ 1 x) (and) #f) #f)
--> (if (not (= x 0)) (if (/ 1 x) #t #f) #f)
```

显然最终求值结果变成了`#t`，而不是`1/x`。

`or`语句的定义方式和`and`类似，不过我们需要引入临时变量用以保存子项的求值结果。

```scheme
(define-syntax or
    (syntax-rules ()
        [(_) #f]
        [(_ e) e]
        [(_ e1 e2 e3 ...)
            (let ([t e1])
                (if t t (or e2 e3 ...)))]))
```

和正常的`let`、`lambda`的绑定变量一样，`模板`中创建的新变量同样只有在模板的词法范围内才有效。
哪怕表达式`e2 e3 ...`中也存在对变量`t`的访问，它们得到的`t`的值也不会是模板中的绑定值（Scheme一般通过自动修改变量名达到该目的）。

和`and`一样，我们也尝试给出一个简化版的`or`。

```scheme
(define-syntax or
    (syntax-rules ()
        [(_) #f]
        [(_ e1 e2 ...)
            (let ([t e1])
                (if t t (or e2 ...)))]))
```

以上实现仍然是错误的！错误原因就作为习题留给读者去思考。

## [习题及解答](https://github.com/jack-ji/scheme-ex/blob/master/tspl/3.ss)
