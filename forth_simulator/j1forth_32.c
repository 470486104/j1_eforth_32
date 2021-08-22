#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>

static unsigned int t;
static unsigned int n;
static unsigned int d[0x20]; /* data stack */
static unsigned int r[0x20]; /* return stack */
static unsigned int pc;    /* program counter, counts cells */
static unsigned char dsp, rsp; /* point to top entry */
static unsigned int memory[0x8000]; /* ram */
static int sx[4] = { 0, 1, -2, -1 }; /* 2-bit sign extension */

FILE* file = NULL;

static void push(int v) // push v on the data stack
{
    dsp = 0x1f & (dsp + 1);
    d[dsp] = t;
    t = v;
}

static int pop(void) // pop value from the data stack and return it
{
    int v = t;
    t = d[dsp];
    dsp = 0x1f & (dsp - 1);
    return v;
}

/*
* uart字符送出端模拟
* 由于getchar()只能接收换行符\n 所以不适合模拟串口
*/
int sum = -1, i = 0;  // sum为最后一个字符下标，i为目前字符的下标
char c[2000];           // 键盘输入缓冲区，最大存2000个字符
char keyboard_input(void) // 键盘输入缓冲  
{
    char t;
    if (i <= sum)
        t = c[i++];     // 如果键盘输入缓冲区里有字符，则送出一个字符
    else
    {                   // 如果键盘输入缓冲区里没有有字符，则取一行字符，取到的字符串的结尾字符更改换行符'\n'为回车符'\r'
        sum = 0;
        printf("\n请输入：");
        while (1)
        {
            if ((t = getchar()) == '\n')
            {
                c[sum] = '\r';
                break;
            }
            else
                c[sum++] = t;
        }
        i = 0;
        t = c[i++];
    }
    return t;
}

static void execute(int entrypoint)  // 指令执行
{
    int _pc, _t;
    int insn = entrypoint; // first insn: "call entrypoint"
    do {
        _pc = pc + 1;
        if (insn & 0x80000000) { // literal
            push(insn & 0x7fffffff);
        }
        else {
            int target = insn & 0x1fffffff;
            switch (insn >> 29) {
            case 0: // jump
                _pc = target;
                break;
            case 1: // conditional jump
                if (pop() == 0)
                    _pc = target;
                break;
            case 2: // call
                rsp = 31 & (rsp + 1);
                r[rsp] = _pc << 2;
                _pc = target;
                break;
            case 3: // alu
                if (insn & 0x1000) {/* r->pc */
                    _pc = r[rsp] >> 2;
                }
                n = d[dsp];
                switch ((insn >> 8) & 0xf) {
                case 0:   _t = t; break; /* noop */
                case 1:   _t = n; break; /* copy */
                case 2:   _t = t + n; break; /* + */
                case 3:   _t = t & n; break; /* and */
                case 4:   _t = t | n; break; /* or */
                case 5:   _t = t ^ n; break; /* xor */
                case 6:   _t = ~t; break; /* invert */
                case 7:   _t = -(t == n); break; /* = */
                case 8:   _t = -((signed int)n < (signed int)t); break; /* < */
                case 9:   _t = n >> t; break; /* rshift */   // ***
                case 0xa:  _t = t - 1; break; /* 1- */
                case 0xb:  _t = r[rsp];  break; /* r@ */
                case 0xc:  _t = (t == 0xf0000001) ? 1 : (t == 0xf0000000) ? keyboard_input() : memory[t >> 2]; break; /* @ */
                case 0xd:  _t = n << t; break; /* lshift */
                case 0xe:  _t = (rsp << 8) + dsp; break; /* dsp */
                case 0xf:  _t = -(n < t); break; /* u< */
                }
                dsp = 31 & (dsp + sx[insn & 3]); /* dstack+- */
                rsp = 31 & (rsp + sx[(insn >> 2) & 3]); /* rstack+- */
                if (insn & 0x80) /* t->n */
                    d[dsp] = t;
                if (insn & 0x40) /* t->r */
                    r[rsp] = t;
                if (insn & 0x0020) /* n->[t] */
                    if (t == 0xf0000000)
                        putchar(n);
                    else
                        memory[t >> 2] = n; /* ! */
             
                t = _t;
                break;
            }
        }
        pc = _pc;
        insn = memory[pc];
        if (insn == 0x6000100C)
            printf("");
    } while (1);
}
/* end of cpu */

/* start of i/o demo */


int main()
{
    FILE* f = fopen("D:\\Cygwin\\tmp\\gforth-0.7.3\\j1_32.bin", "rb");  // j1 forth系统的二进制文件
    fread(memory, 0x4000, sizeof(memory[0]), f); 
    fclose(f);
    execute(0x00);
    return 0;
}
