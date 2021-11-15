int foo(int a,int b,int(* a_fptr)(int, int))
{
    return a_fptr(a,b);
}


int moo(char x)
{
    int (*af_ptr)(int ,int ,int(*)(int, int))=foo;
    return 0;
}