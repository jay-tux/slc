class AsmRegToVars(gdb.Command):
    def __init__(self):
        super(AsmRegToVars, self).__init__(
            "vars", gdb.COMMAND_USER
        )

    def invoke(self, arg, from_tty):
        vars = {}
        mapping = { "x": "r8", "y": "r9", "z": "r10", "a": "r11", "b": "r12", "c": "r13" }
        frame = gdb.selected_frame()
        for v in mapping:
            vars[v] = str(frame.read_register(mapping[v]))
        print(vars)
        print(f'Flags: {frame.read_register("eflags")}')

AsmRegToVars()