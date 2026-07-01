int pop(int *sp, int *stack){
    if (*sp == 0){
        return -99999; //FIXME ??
    }
    *sp = *sp - 1;
    return *(stack + *sp);
}
int push(int *sp, int *stack, int value){
    *(stack + *sp) = value;
    *sp = *sp + 1;
    return -99999; //FIXME void is not exist yet
}



int vm_run(int* code, int code_size){
   int stack[1024];
   int sp = 0;
   int pc = 0;
   while (pc < code_size)
   {
       int op = code[pc];
       pc = pc + 1;
       //PushInt
       if (op == 0){
           if (pc < code_size){
            push(&sp, stack, code[pc]);
            pc = pc + 1;
           }
       //Ret
       } else if (op == 1){
           return pop(&sp, stack);
       //Add
       } else if (op == 2){
           int rhs = pop(&sp, stack);
           int lhs = pop(&sp, stack);
           push(&sp, stack, lhs + rhs);
       //Sub
       } else if (op == 3){
           int rhs = pop(&sp, stack);
           int lhs = pop(&sp, stack);
           push(&sp, stack, lhs - rhs);
       //MUL
       } else if (op == 4){
           int rhs = pop(&sp, stack);
           int lhs = pop(&sp, stack);
           push(&sp, stack, lhs * rhs);
       //Div
       } else if (op == 5){
           int rhs = pop(&sp, stack);
           int lhs = pop(&sp, stack);
           push(&sp, stack, lhs / rhs);
       }







   }
   return 0;
}
/*
int main(){
   int program[3];
   program[0] = 0;
   program[1] = 42;
   program[2] = 1;
   return vm_run(program, 3);
}
*/
