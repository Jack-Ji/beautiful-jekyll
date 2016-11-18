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

## 递归进阶

我们已经在上一章介绍了递归的基本形式，下面我们继续深入学习递归的其它知识。

我们知道`let`语句可以创建一个绑定至某个子程序的变量，那么这个被绑定的子程序能否写成递归形式呢？
答案是不行。下面的例子就做了这样的尝试：

```scheme
(let ([sum (lambda (ls)
            (if (null? ls)
                0
                (+ (car ls) (sum (cdr ls)))))])
    (sum '(1 2 3 4 5)))
```

可以看到，绑定至变量`sum`的`lambda`语句在正文中又对`sum`进行了调用。
很遗憾的是，以上代码会抛出异常，因为在`lambda`语句中`sum`是未定义的标识符，
而`let`创建的绑定变量`sum`只有在其正文部分才是有效的。
为规避这个问题，我们可以在调用`sum`时将自身作为参数传递给它，这样一来`lambda`语句中的`sum`就是个有效变量了。

```scheme
(let ([sum (lambda (sum ls)
            (if (null? ls)
                0
                (+ (car ls) (sum sum (cdr ls)))))])
    (sum sum '(1 2 3 4 5)))
```

看起来很怪异的代码是吧？实际上我们有更好的方式来些这样的代码，那就是用`letrec`语句。
`letrec`的语法结构与`let`一模一样：

```scheme
(letrec ((var expr) ...) body1 body2 ...)
```

`letrec`和`let`不一样的地方在于绑定变量的有效范围不止包含其正文部分，还包含了其初始化表达式。
因此，我们可以用非常直观的方式重写上面的代码：

```scheme
(letrec ([sum (lambda (ls)
            (if (null? ls)
                0
                (+ (car ls) (sum (cdr ls)))))])
    (sum '(1 2 3 4 5)))
```

使用`letrec`我们可以定义多个互相交叉调用的递归函数，以上一章的习题2.8.6为例来定义子程序`even?`和`odd?`：

```scheme
(letrec ([even?
            (lambda (x)
                (or (= x 0)
                    (odd? (- x 1))))]
         [odd?
            (lambda (x)
                (and (not (= x 0))
                     (even? (- x 1))))])
    (list (even? 20) (odd? 20))) => (#t #f)
```

在使用`letrec`的代码中，绑定变量的初始化表达式通常都是`lambda`语句，尽管这并不是必须的。
需要注意的是`letrec`的绑定变量的初始化表达式必须遵循和`let`一样的规定——**初始化表达式能够在不考虑任何绑定变量的情况下被正常求值**。
也许有朋友要问了，不考虑任何绑定变量还怎么能递归调用自身呢？
让我们回忆一下上一章介绍`lambda`语句的章节——`lambda`语句的正文部分出现任何未知变量都是合法的，
因为只有在对其进行调用时才会去检查变量是否合法（正因Scheme的这种极大的灵活性，函数内联这种优化方式很难用到Scheme实现中）。

以下代码是合法的`letrec`语句：

```scheme
(letrec ([f (lambda () (+ x 2))]
         [x 1])
    (f)) => 3
```

以下代码则是不合法的：

```scheme
(letrec ([y (+ x 2)]
         [x 1])
    y)
```

上面的代码会抛出异常，因为`x`在初始化表达式中是无效的。

`letrec`通常被用来作为命名空间来定义一些内部函数，主要是为了不让它与顶层的函数定义相冲突。
下面是`list?`的定义（该定义实现了习题2.9.8中提到的循环检测算法）：

```scheme
(define list?
    (lambda (x)
        (letrec ([race (lambda (h t)
                         (if (pair? h)
                             (let ([h (cdr h)])
                                (if (pair? h)
                                    (and (not (eq? h t))
                                         (race (cdr h) (cdr t)))
                                    (null? h)))
                             (null? h)))])
            (race x x))))
```

当一个内部递归函数只被调用一次时（就像上面的`race`函数），我们可以使用`带命名的let表达式`，其基本形式如下：

```scheme
(let name ((var expr) ...)
    body1 body2)
```

可以看到，`带命名的let表达式`的语法结构与普通的`let`表达式基本相同，主要区别是多了个一个`name`。
该`name`会被绑定至以其正文为函数体、以其绑定变量为参数的子程序，其有效范围是`let`的正文部分。
下面是简化后的`list?`函数：

```scheme
(define list?
    (lambda (x)
        (let race [(h x) (t x)]
            (if (pair? h)
                 (let ([h (cdr h)])
                    (if (pair? h)
                        (and (not (eq? h t))
                             (race (cdr h) (cdr t)))
                        (null? h)))
                (null? h)))))
```

按照我们之前的探讨，`let`可以被改写为直接调用以`let`的正文为函数体的`临时函数`，参数即为绑定变量。
`带命名的let表达式`的改写方式与之类似，只不过我们我们的`临时函数`可以是递归形式的。
下面是通过`letrec`实现`带命名的let表达式`的两种方式：

```scheme
((letrec ((name (lambda (var ...) body1 body2 ...))) name)
    expr ...)

(letrec ((name (lambda (var ...) body1 body2 ...))) 
    (name expr ...))
```

注意上面的第二种改写方式需要确保`expr`中没有同样名为`name`的变量。

我们在上一章中讲过，有些递归本质上其实与c/c++、java中的循环一样，下面我们研究这种递归形式。
当函数调用语句出现在`lambda`表达式的`尾巴部分`（后面会详细解释），我们称这种函数调用方式为`尾调用`。
Scheme规范要求每个Scheme实现都必须将`尾调用`处理成`goto`、`jump`等跳转语句。
而当递归函数中的递归调用同样采用了`尾调用`时，我们称其为`尾递归`。
由于`尾调用`实际上被翻译成为跳转语句，`尾递归`就很自然的形成了一个循环结构。

下面我们该解释什么叫做`lambda`表达式的`尾巴部分`了。
其实很简单，`lambda`表达式中即将返回值的函数调用即为其`尾巴部分`（也就说`尾调用`的返回值即为`lambda`语句的调用结果）。
举例来讲，`lambda`正文的最后一条语句、`if`语句的两个子项、`and`和`or`语句的最后一项、
`let`和`letrec`正文的最后一条语句都可能是`尾调用`。
下面的实例代码中所有对函数`f`的调用都是`尾调用`，所有对函数`g`的调用则不是：

```scheme
(lambda () (f (g)))
(lambda () (if (g) (f) (f)))
(lambda () (let ([x 4]) (f)))
(lambda () (or (g) (f)))
```

基于**递归**和**带命名的let**我们可以实现很多有用的算法，无论是使用循环、递归还是部分循环和部分递归，
Scheme程序员只使用一种语法结构就达到了上述目的，是不是很完美呢？

下面给出两个通过`带命名的let`实现的阶乘运算函数。
第一个直接基于阶乘的定义`n!=n*(n-1)`、`0!=1`实现了一个很简单的递归运算：

```scheme
(define factorial
    (lambda (n)
        (let fact ([i n])
            (if (= i 0)
                1
                (* i (fact (- i 1)))))))

(factorial 0) => 1
(factorial 1) => 1
(factorial 2) => 2
(factorial 3) => 6
(factorial 10) => 362880
```

第二个则通过尾递归实现了`n!=n*(n-1)*(n-2)*...*1`，在递归期间通过变量`a`记录累积计算的结果:

```scheme
(define factorial
    (lambda (n)
        (let fact ([i n] [a 1])
            (if (= i 0)
                a
                (fact (- i 1) (* a i))))))
```

另外一个类似的例子是计算菲波那切数列的第n项。
菲波那切数列是一个特殊的数字序列，其中每个数字都是前两个数字的和，例如`0、1、1、2、3、5、8、13`。
很显然，计算数列的第n项可以很直接的用递归函数实现：

```scheme
(define fibonacci
    (lambda (n)
        (let fib ([i n])
            (cond
                [(= i 0) 0]
                [(= i 1) 1]
                [else (+ (fib (- n 1)) (fib (- n 2)))]))))

(fibonacci 0) => 0
(fibonacci 1) => 1
(fibonacci 2) => 1
(fibonacci 3) => 2
(fibonacci 4) => 3
(fibonacci 5) => 5
(fibonacci 6) => 8
(fibonacci 20) => 6765
(fibonacci 30) => 832040
```


## [习题及解答]=> (https://github.com/jack-ji/scheme-ex/blob/master/tspl/3.ss)
