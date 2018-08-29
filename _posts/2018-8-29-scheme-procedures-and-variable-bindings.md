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
syntax: variable
returns: the value of variable
```


