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

`letrec`通常被用来作为命名空间来定义一些内部函数，主要是为了不让它与上一层的函数定义相冲突。
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
                [else (+ (fib (- i 1)) (fib (- i 2)))]))))

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

上面的递归函数在非基准情形下需要两次递归调用以得到指定位置的前两个数字，这种递归方式也被称为`双递归`。
例如，为计算`(fibonacci 4)`，需要先计算`(fibonacci 3)`和`(fibonacci 2)`，
为计算`(fibonacci 3)`则需要先计算`(fibonacci 2)`和`(fibonacci 1)`，
为计算`(fibonacci 2)`则需要先计算`(fibonacci 1)`和`(fibonacci 0)`。
很显然，这种计算方式的效率非常低，随着参数`n`的增加函数的效率会急剧降低。
另一种较为高效的计算方式是参照`factorial`函数的第二个实现，将累计计算的结果作为参数传递给递归函数：

```scheme
(define fibonacci
    (lambda (n)
        (if (= n 0)
            0
            (let fib ([i n] [a1 1] [a2 0])
                (if (= i 1)
                    a1
                    (fib (- i 1) (+ a1 a2) a1))))))
```

以上实现使用了尾递归写法，递归或者说循环的次数和`n`的值成线性关系。
我们可以通过跟踪函数的递归过程看出两种实现的区别，下面是双递归版本的跟踪结果：

```scheme
|(fib 5)
| (fib 4)
| |(fib 3)
| | (fib 2)
| | |(fib 1)
| | |1
| | |(fib 0)
| | |0
| | 1
| | (fib 1)
| | 1
| |2
| |(fib 2)
| | (fib 1)
| | 1
| | (fib 0)
| | 0
| |1
| 3
| (fib 3)
| |(fib 2)
| | (fib 1)
| | 1
| | (fib 0)
| | 0
| |1
| |(fib 1)
| |1
| 2
|5
```

值得注意的是`(fib 2)`、`(fib 1)`和`(fib 0)`被计算了不止一次。下面是尾递归版本的跟踪结果：

```scheme
|(fib 5 1 0) 
|(fib 4 1 1) 
|(fib 3 2 1) 
|(fib 2 3 2) 
|(fib 1 5 3) 
|5
```

很不一样是吧？

到目前为止我们定义的递归函数中的递归调用要么都是尾递归，要么都不是。
这并不是必须的，现实中的递归函数经常混合使用尾递归和非尾递归。
下面的`factor`函数的功能是对一个非负整数做质数分解，其中第一个递归调用不是尾递归，第二个递归调用则是尾递归。

```scheme
(define factor
	(lambda (n)
		(let f ([n n] [i 2])
			(cond
				[(>= i n) (list n)]
				[(integer? (/ n i))
				 (cons i (f (/ n i) i))]
				[else (f n (+ i 1))]))))

(factor 0) => 0
(factor 1) => 1
(factor 12) => (2 2 3)
(factor 362880) => (2 2 2 2 2 2 2 2 3 3 3 3 5 5 7)
(factor 9239) => 9239
```

`Chez Scheme`可以通过`trace-let`跟踪带命名的let语句的递归调用过程。
下面是`(factor 120)`的跟踪结果：

```scheme
|(f 120 2)
| (f 60 2)
| |(f 30 2)
| | (f 15 2)
| | (f 15 3)
| | |(f 5 3)
| | |(f 5 4)
| | |(f 5 5)
| | |(5)
| | (3 5)
| |(2 3 5)
| (2 2 3 5)
|(2 2 2 3 5)
(2 2 2 3 5)
```

## Continuation

在对表达式求值的过程中，Scheme需要处理两个基本问题：1）对什么求值；2）如何处理求值结果。
让我们以下面的代码为例来分析Scheme对`(null? x)`的求值处理：

```scheme
(if (null? x) (quote ()) (cdr x))
```

Scheme对上面的表达式求值时必须先计算`(null? x)`的值，然后再根据其结果决定对`(quote ())`还是`(cdr x)`求值。
这就引出了以上两个基本问题的答案：1）对什么求值——`(null? x)`；2）如何处理求值结果——决定对`(quote ())`还是`(cdr x)`求值。
我们称上面的第二条，也就是**“如何处理求值结果”**为表达式求值的_**continuation**_。

根据以上所述，Scheme在对任何表达式求值的过程中都有一个`continuation`要完成。假设变量`x`的值为`(a b c)`，
Scheme在对表达式`(if (null? x) (quote ()) (cdr x))`求值时需要完成下面6个值的`continuation`。

1. `(if (null? x) (quote ()) (cdr x))`的值；
2. `(null? x)`的值；
3. `null?`的值；
4. `x`的值；
5. `cdr`的值；
6. `x`的值；

`(cdr x)`没有被列出是因为它的`continuation`和`(if (null? x) (quote ()) (cdr x))`的`continuation`是一样的。

下面重头戏来了，**Scheme允许通过`call/cc`捕捉任何表达式的continuation。`call/cc`只接受一个实参`p`，`p`必须为参数个数为1的子程序。
`call/cc`会将从当前执行上下文中捕获到的`continuation`作为参数传递给`p`。`continuation`的具体表示形式为一个子程序`k`。
当`k`被传递了某个值进行调用时，该值会被传递给`call/cc`表达式自身的`continuation`，最终该值会成为`call/cc`表达式的求值结果。**

考虑下面的简单实例：

```scheme
(call/cc
    (lambda (k)
        (* 5 4))) => 20

(call/cc
    (lambda (k)
        (* 5 (k 4)))) => 4

(+ 2
   (call/cc
       (lambda (k)
           (* 5 (k 4))))) => 6
```

上面的`call/cc`语句都将捕捉到的`continuation`绑定至了变量`k`。
在第一个例子中，`k`未被使用，因此`call/cc`表达式的值为5和4的乘积。
在第二个例子中，`k`调用出现在乘法运算之前，因此`call/cc`表达式的值为4。
在第三个例子中，`call/cc`表达式的值与2相加，因此结果为6。

下面是个稍复杂点的例子，它展示了如何通过`call/cc`强制中断递归并返回指定的值，我们称之为`非本地返回`。

```scheme
(define product
    (lambda (ls)
        (call/cc
            (lambda (break)
                (let f ([ls ls])
                    (cond
                        [(null? ls) 1]
                        [(= (car ls) 0) (break 0)]
                        [else (* car ls) (f (cdr ls))]))))))

(product '(1 2 3 4 5)) => 120
(product '(7 3 8 0 1 9 5)) => 0
```

可以看到，当检测到`ls`中某个子项的值为0时，`非本地返回`能够立刻指定`call/cc`表达式的返回值，从而跳过了无用的乘法运算。

上面给出的例子都在`call/cc`的参数的函数体中调用了`continuation`，下面给出一个在`call/cc`表达式之外调用`continuation`的例子：

```scheme
(let ([x (call/cc (lambda (k) k))])
    (x (lambda (ignore) "hi"))) => "hi"
```

以上代码中的`call/cc`捕获的`continuation`代表的信息是：“将求值结果绑定给变量`x`，然后调用`x`，参数为`(lambda (ignore) "hi")`”。
`(lambda (k) k)`实际上直接返回了`x`自身`continuation`。
而`(x (lambda (ignore) "hi"))`则相当于将`x`绑定至新的函数，并且将自身作为参数对函数进行了调用。

下面是外部调用`continuation`的另一种写法。在所有规模与之相同的Scheme代码中，下面的代码可能是最让人难以理解的了。
尽管你可能很容易就能猜出来它的求值结果，但是想明白具体求值过程则需要仔细的思考才行。

```scheme
(((call/cc (lambda (k) k)) (lambda (x) x)) "HEY!") => "HEY!"
```

和前一个的例子一样，`call/cc`表达式返回了自身的`continuation`。
然后在调用该`continuation`时传递了函数表达式`(lambda (x) x)`。
接着还是像前一个例子一样，函数`(lambda (x) x)`以自身为参数对自己进行了调用，调用结果是返回自身。
最后，字符串"HEY!"被传递给了函数`(lambda (x) x)`，从而得到了最终的求值结果"HEY!"。

外部调用`continuation`的代码并不总是像上面的示例代码那样晦涩难懂。
下面的`factorial`函数在基准情形下将`continuation`保存至了全局变量`retry`。

```scheme
(define retry #f)

(define factorial
    (lambda (x)
        (if (= x 0)
            (call/cc (lambda (k) (set! retry k) 1))
            (* x (factorial (- x 1))))))
```

上面的`factorial`函数和普通的递归函数的主要区别在于对`retry`的赋值。

```scheme
(factorial 4) => 24
(retry 1) => 24
(retry 2) => 48
```

在上例中，`retry`所绑定的`continuation`代表了“将基准值设置为1，然后再乘以2，然后再乘以3，然后再乘以4”。
因此，我们调用`retry`就相当于重新设置了基准值后再计算结果，从而得出了完全不同的值。

基于`continuation`的这个机制我们可以通过`call/cc`实现一个为其它函数添加断点的函数库。
每个断点都会保存一个`continuation`，我们可以通过保存的`continuation`重新从断点起执行被中断的函数。

`continuation`还可以被用于实现各种形式的`多任务处理`。例如，下面的示例代码就基于`continuation`实现了`轻量级多进程`。
由于该模型的调度方式是非抢断的，各个任务需要主动暂停以保证其它任务能够得到执行。

```scheme
(define lwp-list '())
(define lwp
    (lambda (thunk)
        (set! lwp-list (append lwp-list (list thunk)))))

(define start
    (lambda ()
        (let ([p (car lwp-list)])
            (set! lwp-list (cdr lwp-list))
            (p))))

(define pause
    (lambda ()
        (call/cc
            (lambda (k)
                (lwp (lambda () (k #f)))
                (start)))))
```

下面的代码使用了该模型创建了多个任务，每个任务打印一个字符，多个任务协作起来输出了字符串"hey!"。

```scheme
(lwp (lambda () (let f () (pause) (display "h") (f))))
(lwp (lambda () (let f () (pause) (display "e") (f))))
(lwp (lambda () (let f () (pause) (display "y") (f))))
(lwp (lambda () (let f () (pause) (display "!") (f))))
(lwp (lambda () (let f () (pause) (newline) (f))))
(start) => hey!
           hey!
           hey!
           hey!
           ...
```

## Continuation的传递（CPS——Continuation Passing Style）

正如我们在上一节中讨论的，每个表达式的求值都会涉及到一个`continuation`的处理。特别的，函数调用也是如此。
当一个函数以非尾递归的方式调用了另一个函数时，被调用的函数将收到一个新的`continuation`，
该`continuation`负责处理被调用函数返回后的工作并将结果返回给调用方函数的`continuation`。
当使用了尾调用方式时，被调用函数将直接收到调用方函数的`continuation`。

我们能通过以匿名函数为参数的方式将函数调用的`continuation`显式的表现出来。
首先请看下面的代码：

```scheme
(letrec ([f (lambda (x) (cons 'a x))]
         [g (lambda (x) (cons 'b (f x)))]
         [h (lambda (x) (g (cons 'c x)))])
    (cons 'd (h '()))) => (d b a c)
```

函数`f`的`continuation`将符号`b`与传递给其的返回值`cons`起来，然后再将`cons`的结果返回给上一层函数`g`的`continuation`。
由于函数`h`以尾调用方式调用了函数`g`，函数`g`的`continuation`和函数`h`相同，其工作是将符号`d`和传递给其的值`cons`起来。
通过`CPS`，我们可以将上面的代码改写为显示传递和处理`continuation`的实现：

```scheme
(letrec ([f (lambda (x k) (k (cons 'a x)))]
         [g (lambda (x k)
                (f x (lambda (v) (k (cons 'b v)))))]
         [h (lambda (x k) (g (cons 'c x) k))])
    (h '() (lambda (v) (cons 'd v))))
```

和上面讨论的隐式的`continuation`一样，函数`h`直接将自己的`continuation`传递给了函数`g`，内容如下：

```scheme
(lambda (v) (cons 'd v))
```

和前一个例子中的函数`h`和函数`g`的隐式`continuation`一样，该显式`continuation`同样将符号`d`和传递给其的值`cons`起来。
函数`f`的`continuation`的内容如下：

```scheme
(lambda (v) (k (cons 'b v)))
```

该`continuation`的功能是将符号`b`和传递给其的其`cons`起来，然后再将结果传递给函数`g`的`continuation`。

以`CPS`风格编写的代码显然要复杂难懂一些，然而它也带来了一些好处。
首先，显式编写的`continuation`可以被传递多个值，请看下面的例子（我们将在第5章学习更为简易的多返回值语法）： 

```scheme
(define car&cdr
    (lambda (p k)
        (k (car p) (cdr p))))

(car&cdr '(a b c)
    (lambda (x y)
        (list y x))) => ((b c) a)
(car&cdr '(a b c) cons) => (a b c)
(car&cdr '(a b c a d) memv) => (a d)
```

其次，我们可以通过`CPS`为函数添加多个`continuation`。
特别的，我们可以使函数接收`success`和`failure`两个`continuation`，它们分别在函数处理成功和失败时生效。
以下面的`integer-divide`为例，在计算正常的情况下该函数会通过`success`返回商和余数，否则会通过`failure`返回异常信息：

```scheme
(define integer-divide
    (lambda (x y success failure)
        (if (= y 0)
            (failure "divide by zero")
            (let ([q (quotient x y)])
                (success q (- x (* q y)))))))

(integer-divide 10 3 list (lambda (x) x)) => (3 1)
(integer-divide 10 0 list (lambda (x) x)) => "divide by zero"
```

函数`quotient`的功能是计算两个参数相除的商（和C/C++中的`/`操作符的功能类似）。

通过显示的传递成功和失败两种情况的`continuation`可以帮助简化函数调用返回后的处理工作。
更有甚者，我们可以传递多种不同风格的成功和失败的`continuation`，每个`continuation`接受的参数个数和参数类型都不尽相同，
我们将在第12章中的代码示例中应用这种技术。

现在，你也许想问`CPS`和通过`call/cc`捕获的`continuation`之间是什么关系？
实际上，任何使用`call/cc`实现的程序都可被改写为`CPS`风格。下面是将之前的`product`函数改写后的效果：

```scheme
(define product
    (lambda (ls k)
        (let ([break k])
            (let f ([ls ls] [k k])
                (cond
                    [(null? ls) (k 1)]
                    [(= (car ls) 0) (break 0)]
                    [else (f (cdr ls)
                             (lambda (x)
                                (k (* (car ls) x))))])))))

(product '(1 2 3 4 5) (lambda (x) x)) => 120
(product '(7 3 8 0 1 9 5) (lambda (x) x)) => 0
```

## 内部定义

在上一章，我们讨论了顶层定义语法，也就是`define`表达式。
实际上，`define`表达式也可以被作为`lambda`、`let`和`letrec`的正文部分的起始内容。
很显然，这种`define`语句定义的变量的有效范围就是其正文部分，因此我们称这种定义为`内部定义`。

```scheme
(define f (lambda (x) (* x x)))
(let ([x 3])
    (define f (lambda (y) (+ y x)))
    (f 4)) => 7
(f 4) => 16
```

内部定义同样可以使用递归技术。我们可以将上一章的`even?`和`odd?`实例改写如下：

```scheme
(let ()
    (define even?
        (lambda (x)
            (or (= x 0)
                (odd? (- x 1)))))
    (define odd?
        (lambda (x)
            (and (not (= x 0))
                 (even? (- x 1)))))
    (even? 20)) => #t
```

类似的，我们也可以将之前编写的`list?`函数中的`letrec`替换为内部定义：

```scheme
(define list?
    (lambda (x)
        (define race
            (lambda (h t)
                (if (pair? h)
                    (let ([h (cdr h)])
                        (if (pair? h)
                            (and (not (eq? h t))
                                 (race (cdr h) (cdr t)))
                            (null? h)))
                     (null? h))))
         (race x x)))
```

抛掉语法上的差别不提，内部定义和`letrec`的最大差别是`letrec`无法保证多个内部定义按照从左至右的顺序依次被求值，
因此我们不能使用`letrec`完全替代内部定义。
我们可以使用`letrec*`（`let*`与之类似）来保证其绑定语句被严格地按照从左至右的顺序进行求值。
下面是内部定义的一般形式（位于`lambda`、`let`、`letrec`等语句的正文起始部分）：

```scheme
(define var expr0)
...
expr1
expr2
...
```

该定义可被等价转换为以下`letrec*`语句：

```scheme
(letrec* ((var expr0) ...) expr1 expr2 ...)
```

以上`letrec*`语句又可被等价转换为下面的`let`语句：

```scheme
(let ()
    (define var expr0)
    ...
    expr1
    expr2
    ...
)
```

很明显，`letrec*`和内部定义的相互转换是`非对称`的，这是因为`letrec*`可以出现任何表达式能出现的地方，
而内部定义则只能出现在正文的起始部分。因此当我们使用内部定义替换`letrec*`语句时需要引入`let`表达式。

内部定义和`letrec`、`letrec*`的另一个较大的区别是内部定义不光能定义变量，还可以定义语法扩展（我们称其为内部语法扩展）！

```scheme
(let ([x 3])
    (define-syntax set-x!
        (syntax-rules ()
            [(_ e) (set! x e)]))
    (set-x! (+ x x))
    x) => 6
```

内部语法扩展的有效范围同样是包围该定义的语句的正文部分。

内部定义和最外层的顶层定义结合起来为我们提供了一个将程序划分为多个模块的手段。
程序的每个模块都只对外暴露其它模块会使用的接口，内部函数或接口都应被隐藏起来以免误被使用。
下面是一种常用的定义模块的方式：

```scheme
(define export-var #f)

(let ()
    (define var expr)
    ...
    init-expr
    ...
    (set! export-var export-val)
)
```

首先，我们使用`define`在全局命名空间中添加了需要对外暴露的变量名称。
然后，我们在`let`语句的正文起始部分定义了内部使用的变量，并通过表达式`init-expr ...`完成必要的初始化工作。
最后，我们通过`set!`表达式设置需要对外暴露的变量的值。

以上定义方式的优点之一是可以在开发模块的过程中将最外层的`let`语句注释或删除掉，以便开发者在交互环境中进行测试。
当然了，这种定义方式也有缺点，我们在下一节中会讲到。

## [习题及解答](https://github.com/jack-ji/scheme-ex/blob/master/tspl/3.ss)
