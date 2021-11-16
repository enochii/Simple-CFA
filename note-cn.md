## Note

这里说一个比较好玩的点。

```c
int foo(int a,int b,struct fptr af_ptr)
{
    return af_ptr.p_fptr(a,b);
}
void make_simple_alias(struct fptr *a,struct fptr * b)
{
    a->p_fptr=b->p_fptr;
}
int clever() {
  
    int (*af_ptr)(int ,int ,struct fptr)=0;
    struct fptr tf_ptr={0};
    struct fptr sf_ptr={0};
    tf_ptr.p_fptr=minus;
    sf_ptr.p_fptr=plus;
    af_ptr=foo;
    make_simple_alias(&tf_ptr,&sf_ptr);
    unsigned result = af_ptr(1, 2,tf_ptr);
    return 0;
}
```

假设我们先分析 f ，到达调用点后，因为对 g 的调用可能会改变 x 指针集，或者是 f 在这个调用点的状态，如果我们继续处理后续语句，会造成不精确。

这个是 `test22.c`，在 `make_simple_alias` 因为参数传递使得 `make_simple_alias` 的 entry node 状态改变，我们会把 `make_simple_alias` 加入 worklist 。对于 `clever`，如果继续走下去会有不精确的数据流（认为 `tf_ptr.p_fptr` 还可能指向 `minus`，然而在这个函数调用后只能指向 `plus`）。所以我们会类似 `*x=y` x 指针集为空时一样处理，即把调用点所有指针集清空，然后中断对 `clever` 的处理。

另一方面，我们希望处理完 `make_simple_alias` 后再来处理 `clever`，所以我们会把 `clever` 也加入 worklist （所以 worklist 应该有两项 `make_simple_alias`（前）和 `clever`（后））。故我们的 worklist 数据结构要**提供 FIFO 的语义**。否则如果下一个 worklist pop 还是拿到 `clever` ，那么还是会出现不精确的问题。一开始我用 set 发现也对了，这是因为函数声明顺序造成了指针的大小差异，而这个又是 set 的排序依据，刚好我们的 `clever` 会后 pop 。所以对于 set 换成 `test/ssch32.c`就会有问题。

由于测试样例不足，其实还有一些 case 没处理到，比如有调用链：`F -> G -> H`，先处理 F ，调用 G ，队列变成：
  
  G F

处理 G ，调用 H，队列变成：

  F H G

这时候上回 G 的数据流变化还没经过 return 加入 F 对 G 的调用点（我们 abort F 的地方），所以会损失精度。说到底需要更复杂的机制，从而对于 x 调用 y，如果 y 会影响 x 的状态，**要在 x 重新处理前处理 y** ，然后通过 return 把信息要回来。FIFO 语义只能保证简单情况下 work ，复杂情况是不足的。这玩意要动态地计算一个拓扑序？

当然不处理会丢精度，但不会影响 soundness 。这个好像也不会影响单调性？
