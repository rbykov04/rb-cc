int pop(int *sp, int *stack){
    if (*sp == 0){
        return -99999; //FIXME ??
    }
    *sp = *sp - 1;
    return *(stack + *sp);
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
            stack[sp] = code[pc];
            pc = pc + 1;
            sp = sp + 1;
           }
       //Ret
       } else if (op == 1){
           if (sp == 0){
               return 0;
           }
           return stack[sp - 1];
       //Add
       } else if (op == 2){
           int lhs = pop(&sp, stack);
           int rhs = pop(&sp, stack);

           stack[sp] = lhs + rhs;
           sp = sp + 1;
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
