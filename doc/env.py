class Env(dict):
    "An environment: a dict of {'var':val} pairs, with an outer Env."
    def __init__(self, parms=(), args=(), outer=None):
        self.update(zip(parms,args))
        self.outer = outer

    def find(self, var):
        "Find the innermost Env where var appears."
        return self if var in self else self.outer.find(var)

class Struct(Env):
    "A structure that can have any fields defined."
    def __init__(self, **entries): 
        self.__dict__.update(entries)

    def __repr__(self):
        args = ['%s=%s' % (k, repr(v)) for (k,v) in vars(self).items()]
        return 'Struct(%s)' % ', '.join(args)



