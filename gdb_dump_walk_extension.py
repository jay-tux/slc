class AsmDumpWalkXtend(gdb.Command):
    def __init__(self):
        super(AsmDumpWalkXtend, self).__init__(
            "asm_dump_walk", gdb.COMMAND_USER
        )

    def invoke(self, arg, from_tty):
        gdb.execute("si")
        gdb.execute("i r r8 r9 r10 r11 r12 r13 rax al eflags")

AsmDumpWalkXtend()
