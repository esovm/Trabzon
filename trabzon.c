
#define _BSD_SOURCE
#define _POSIX_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <elf.h>

#define MEMORY_SIZE 30000

enum ins_t {
    IMOVE, IMUTATE,
    IIN, IOUT,
    IBRANCH, IJUMP,
    IHALT, ICLEAR,
    IADD, INOP
};

enum op_t {
    OPANY, OPCONSTANT,
    OPSTORE, OPLOAD,
    OPLOAD_NEG
};

enum mode_t {
    MOPEN, MFUNCTION,
    MSTANDALONE
};

struct prg_t {
    int max, count;
    struct {
        enum ins_t ins_t;
        long operand;
    } * ins_t;
    int markers_max, markers_count;
    long * markers;
};

struct pattern_t {
    enum ins_t ins_t;
    enum op_t op_t;
    long operand;
};

struct opt_t {
    const char * name;
    struct pattern_t * sequence;
    struct pattern_t * replacement;
};

struct asm_t {
    int size, fill;
    uint8_t code[];
};

static struct opt_t opt_clear = {
    .name = "Cell clear",
    .sequence = (struct pattern_t []) {
        {IBRANCH},
        {IMUTATE, OPCONSTANT, -1},
        {IJUMP},
        {INOP}
    },
    .replacement = (struct pattern_t []) {
        {ICLEAR},
        {INOP},
        {INOP}
    }
};

static struct opt_t opt_add = {
    .name = "Cell add",
    .sequence = (struct pattern_t []) {
        {IBRANCH},
        {IMUTATE, OPCONSTANT, -1},
        {IMOVE, OPSTORE, 0},
        {IMUTATE, OPCONSTANT, 1},
        {IMOVE, OPLOAD_NEG, 0},
        {IJUMP},
        {INOP}
    },
    .replacement = (struct pattern_t []) {
        {IADD, OPLOAD, 0},
        {ICLEAR},
        {INOP},
        {INOP},
        {INOP},
        {INOP}
    }
};

static const char * const names[] = {
    "MOVE", "MUTATE", "IN", "OUT", "BRANCH",
    "JUMP", "HALT", "CLEAR", "ADD", "NOP"
};

static const int arity[] = {
    1, 1, 0, 0, 1, 1, 0, 0, 0
};

static void prgfree(struct prg_t * p) {
    free(p->ins_t);
    free(p->markers);
}

static void prgmark(struct prg_t * p) {
    if (p->markers_count == p->markers_max) {
        if (p->markers_max == 0)
            p->markers_max = 16;
        else
            p->markers_max *= 2;
        int size = sizeof(p->markers[0]) * p->markers_max;
        p->markers = realloc(p->markers, size);
    }
    p->markers[p->markers_count++] = p->count;
}

static long prgunmark(struct prg_t * p) {
    if (p->markers_count > 0)
        return p->markers[--p->markers_count];
    else
        return -1;
}

static void prgadd(struct prg_t * p, enum ins_t ins_t, long operand) {
    int size;
    if (p->count == p->max) {
        if (p->max == 0)
            p->max = 256;
        else
            p->max *= 2;
        size = sizeof(p->ins_t[0]) * p->max;
        p->ins_t = realloc(p->ins_t, size);
    }
    switch (ins_t) {
        case IBRANCH:
            prgmark(p);
            break;
        case IJUMP: {
                long sibling = prgunmark(p);
                if (sibling < 0) {
                    fprintf(stderr, "trabzon: unmatched ']'\n");
                    abort();
                }
                p->ins_t[sibling].operand = p->count + 1;
                operand = sibling;
            } break;
        default:
            break;
    }
    p->ins_t[p->count].ins_t = ins_t;
    p->ins_t[p->count].operand = operand;
    p->count++;
}

static void prgparse(struct prg_t * p, FILE * in) {
    int c;
    while ((c = fgetc(in)) != EOF) {
        switch (c) {
            case '+':
                prgadd(p, IMUTATE, 1);
                break;
            case '-':
                prgadd(p, IMUTATE, -1);
                break;
            case '>':
                prgadd(p, IMOVE, 1);
                break;
            case '<':
                prgadd(p, IMOVE, -1);
                break;
            case '.':
                prgadd(p, IOUT, 0);
                break;
            case ',':
                prgadd(p, IIN, 0);
                break;
            case '[':
                prgadd(p, IBRANCH, 0);
                break;
            case ']':
                prgadd(p, IJUMP, 0);
                break;
            default:
                break;
        }
    }
    if (p->markers_count > 0) {
        fprintf(stderr, "trabzon: unmatched ']'\n");
        abort();
    }
    prgadd(p, IHALT, 0);
}

static void prgstrip_nops(struct prg_t * p) {
    struct prg_t copy = {0, 0, NULL, 0, 0, NULL};
    int i = 0;
    for (; i < p->count; i++)
        if (p->ins_t[i].ins_t != INOP)
            prgadd(&copy, p->ins_t[i].ins_t, p->ins_t[i].operand);
    prgfree(p);
    *p = copy;
}

static void optreplace(struct prg_t * p, long i, struct opt_t * o, long * vars) {
    long n = 0;
    for (; o->sequence[n].ins_t != INOP; n++) {
        p->ins_t[i + n].ins_t = o->replacement[n].ins_t;
        switch (o->replacement[n].op_t) {
            case OPCONSTANT:
                p->ins_t[i + n].operand = o->replacement[n].operand;
                break;
            case OPLOAD:
                p->ins_t[i + n].operand = vars[o->replacement[n].operand];
                break;
            case OPLOAD_NEG:
                p->ins_t[i + n].operand = -vars[o->replacement[n].operand];
                break;
            case OPANY:
                break;
            case OPSTORE:
                fprintf(stderr, "trabzon: internal error\n");
                abort();
                break;
        }
    }
}

static long optapply(struct prg_t * p, struct opt_t * o) {
    long count = 0, variables[16] = {0};
    int i = 0;
    for (int i = 0; i < p->count; i++) {
        long length = 0;
        for (int n = 0; o->sequence[n].ins_t != INOP && i + n < p->count; n++) {
            enum ins_t ins_t = p->ins_t[i + n].ins_t;
            enum op_t o_type;
            long operand = p->ins_t[i + n].operand, o_operand;
            if (ins_t != o->sequence[n].ins_t) {
                length = 0;
                break;
            }
            length++;
            o_operand = o->sequence[n].operand;
            o_type = o->sequence[n].op_t;
            switch (o_type) {
                case OPCONSTANT:
                    if (operand != o_operand)
                        length = 0;
                    break;
                case OPSTORE:
                    variables[o_operand] = operand;
                    break;
                case OPLOAD:
                    if (operand != variables[o_operand])
                        length = 0;
                    break;
                case OPLOAD_NEG:
                    if (operand != -variables[o_operand])
                        length = 0;
                    break;
                case OPANY:
                    break;
            }
            if (length == 0)
                break;
        }
        if (length > 0) {
            optreplace(p, i, o, variables);
            i += length;
            count++;
        }
    }
    if (count > 0)
        prgstrip_nops(p);
    return count;
}

static void prgcompress(struct prg_t * p) {
    for (int i = 0; i < p->count; i++) {
        if (p->ins_t[i].ins_t == IMOVE || p->ins_t[i].ins_t == IMUTATE) {
            int f = i + 1;
            while (p->ins_t[i].ins_t == p->ins_t[f].ins_t) {
                p->ins_t[f].ins_t = INOP;
                p->ins_t[i].operand += p->ins_t[f].operand;
                f++;
            }
            if (p->ins_t[i].operand == 0)
                p->ins_t[i].ins_t = INOP;
        }
    }
    prgstrip_nops(p);
}

static void prgoptimize(struct prg_t * p) {
    long count = 0;
    prgcompress(p);
    do {
        count = 0;
        count += optapply(p, &opt_clear);
        count += optapply(p, &opt_add);
    } while (count > 0);
}

static struct asm_t * asmcreate(void) {
    long page_size = sysconf(_SC_PAGESIZE);
    int prot = PROT_READ | PROT_WRITE, flags = MAP_ANONYMOUS | MAP_PRIVATE, size = page_size * 1024;
    struct asm_t * buf = mmap(NULL, size, prot, flags, -1, 0);
    buf->size = size;
    return buf;
}

static void asmfree(struct asm_t * buf) {
    munmap(buf, buf->size);
}

static void asmfinalize(struct asm_t * buf) {
    mprotect(buf, buf->size, PROT_READ | PROT_EXEC);
}

static void asmins(struct asm_t * buf, int size, uint64_t ins_t) {
    for (int i = size - 1; i >= 0; i--)
        buf->code[buf->fill++] = (ins_t >> (i * 8)) & 0xff;
}

static void asmimmediate(struct asm_t * buf, int size, const void * value) {
    memcpy(buf->code + buf->fill, value, size);
    buf->fill += size;
}

static void asmsyscall(struct asm_t * buf, int syscall) {
    if (syscall == 0)
        asmins(buf, 2, 0x31C0);
    else if (syscall == 1)
        asmins(buf, 3, 0x4C89E0);
    else {
        asmins(buf, 1, 0xB8);
        uint32_t n = syscall;
        asmimmediate(buf, 4, &n);
    }
    asmins(buf, 2, 0x0F05);
}

static struct asm_t * compile(const struct prg_t * prg_t, enum mode_t mode_t) {
    uint32_t memory_size = MEMORY_SIZE;
    struct asm_t * buf = asmcreate();
    if (mode_t == MFUNCTION) {
        asmins(buf, 1, 0x53);
        asmins(buf, 2, 0x4154);
    }
    asmins(buf, 2, 0x31dB);
    asmins(buf, 6, 0x41BC01000000);
    asmins(buf, 3, 0x4881EC);
    asmimmediate(buf, 4, &memory_size);
    asmins(buf, 3, 0x4889E6);
    asmins(buf, 5, 0xBA01000000);
    asmins(buf, 2, 0x30C0);
    asmins(buf, 3, 0x4889E7);
    asmins(buf, 1, 0xB9);
    asmimmediate(buf, 4, &memory_size);
    asmins(buf, 2, 0xF3AA);
    uint32_t * table = malloc(sizeof(table[0]) * prg_t->count);
    for (int i = 0; i < prg_t->count; i++) {
        enum ins_t ins_t = prg_t->ins_t[i].ins_t;
        long operand = prg_t->ins_t[i].operand;
        table[i] = buf->fill;
        switch (ins_t) {
            case IMOVE:
                if (operand > -256 && operand < 256) {
                    if (operand > 0)
                        asmins(buf, 3, 0x4883C6);
                    else {
                        operand *= -1;
                        asmins(buf, 3, 0x4883EE);
                    }
                    asmimmediate(buf, 1, &operand);
                } else {
                    if (operand > 0)
                        asmins(buf, 3, 0x4881C6);
                    else {
                        operand *= -1;
                        asmins(buf, 3, 0x4881EE);
                    }
                    asmimmediate(buf, 4, &operand);
                }
                break;
            case IMUTATE:
                if (operand > 0)
                    asmins(buf, 2, 0x8006);
                else {
                    operand *= -1;
                    asmins(buf, 2, 0x802E);
                }
                asmimmediate(buf, 1, &operand);
                break;
            case ICLEAR:
                asmins(buf, 2, 0x881e);
                break;
            case IIN:
                asmins(buf, 3, 0x4831FF);
                asmsyscall(buf, SYS_read);
                break;
            case IOUT:
                asmins(buf, 3, 0x4C89E7);
                asmsyscall(buf, SYS_write);
                break;
            case IBRANCH: {
                    uint32_t delta = 0;
                    asmins(buf, 2, 0x381E);
                    asmins(buf, 2, 0x0F84);
                    asmimmediate(buf, 4, &delta);
                } break;
            case IJUMP: {
                    uint32_t delta = table[operand];
                    delta -= buf->fill + 5;
                    asmins(buf, 1, 0xE9);
                    asmimmediate(buf, 4, &delta);
                    void * jz = &buf->code[table[operand] + 4];
                    uint32_t patch = buf->fill - table[operand] - 8;
                    memcpy(jz, &patch, 4);
                } break;
            case IADD: {
                    asmins(buf, 2, 0x8A06);
                    uint32_t delta = operand;
                    if (operand >= -128 && operand < 127) {
                        asmins(buf, 2, 0x0046);
                        asmimmediate(buf, 1, &delta);
                    } else {
                        asmins(buf, 2, 0x0086);
                        asmimmediate(buf, 4, &delta);
                    }
                } break;
            case IHALT:
                if (mode_t == MFUNCTION) {
                    asmins(buf, 3, 0x4881C4);
                    asmimmediate(buf, 4, &memory_size);
                    asmins(buf, 2, 0x415C);
                    asmins(buf, 1, 0x5B);
                    asmins(buf, 1, 0xC3);
                } else if (mode_t == MSTANDALONE) {
                    asmins(buf, 3, 0x4831FF);
                    asmsyscall(buf, SYS_exit);
                }
                break;
            case INOP:
                break;
        }
    }
    free(table);
    asmfinalize(buf);
    return buf;
}

int main(int argc, char * argv[]) {
    struct prg_t prg = {0, 0, NULL, 0, 0, NULL};
    struct asm_t * buf;
    FILE * source;
    void (*run)(void);
    if (argc < 2) {
        fprintf(stderr, "trabzon: no input files\n");
        abort();
    }
    source = fopen(argv[1], "r");
    if (source == NULL) {
        fprintf(stderr, "trabzon: couldn't open input file.\n");
        abort();
    }
    prgparse(&prg, source);
    prgoptimize(&prg);
    fclose(source);
    buf = compile(&prg, MFUNCTION);
    run = (void *)buf->code;
    run();
    asmfree(buf);
    prgfree(&prg);
    return 0;
}
