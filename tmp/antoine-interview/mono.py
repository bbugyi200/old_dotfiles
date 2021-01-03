import sys


def to_int(s):
    if s == "":
        return 1
    if s == "-":
        return -1
    return int(s)


class Mono:
    def __init__(self, c=0, a=0, b=0):
        """A mono object represents cX^aY^b."""
        # TODO: re-factor to use exponent?
        self.c = c
        self.a = a
        self.b = b

    def exponent(self):
        return Exponent(self.a, self.b)

    def make(self, typ, value):
        value = to_int(value)
        if typ == "1":
            print("setting constant term:", value)
            self.c = value
        if typ == "x":
            print("setting x term:", value)
            self.a = value
        if type == "y":
            print("setting y term:", value)
            self.b = value

    def add(self, other):
        """Add two Mono.
        TODO: Probably assert the power are the same"""
        return Mono(self.c, other.c, self.a, self.b)

    def mult(self, other):
        """Multiply two Mono"""
        return Mono(self.c * other.c, self.a + other.a, self.b + other.b)

    def __str__(self):
        """Take care of cases."""
        return "%sX^%sY^%s" % (self.c, self.a, self.b)

    @staticmethod
    def from_string(s):
        """There is probably a smart way to do that and less disgusting..."""
        print("got mono input:", s)
        m = Mono()
        # Get the constant part
        current = ""
        i = 0
        for i in range(0, len(s)):
            if s[i] == "x" or s[i] == "y":
                break
            current += s[i]
        m.make("1", current)
        current = ""
        for j in range(i, len(s)):
            if s[j] == "x" or s[j] == "y":
                break
            current += s[j]
        m.make(s[i], current)

        return m


class Exponent:
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def to_key(self):
        return "%s|%s" % (self.a, self.b)

    @staticmethod
    def from_key(key):
        return Exponent(*[int(_) for _ in key.split('|')])


class Poly:
    def __init__(self):
        """Dictionary of Exponent to Mono."""
        self.monos = {}

    def add(self, mono):
        """We add to existing mono if exponent exists.
        Otherwise, create it."""
        if not mono:
            return
        k = mono.exponent().to_key()
        if k not in self.monos:
            self.monos[k] = mono
            return
        self.monos[k] = self.monos[k].add(mono)

    @staticmethod
    def from_string(s):
        # We iterate over the string until we find + or - and accumulate current mono string
        p = Poly()
        current = ""
        for c in s:
            if c == "+":
                m = Mono.from_string(current)
                if m:
                    print("Adding mono:", str(m))
                # p.add(m))
                current = ""
                continue
            if c == "-":
                m = Mono.from_string(current)
                if m:
                    print("Adding mono:", str(m))
                # p.add(m)
                current = "-"
                continue
            current += c
        # Add the last
        m = Mono.from_string(current)
        if m:
            print("Adding mono:", str(m))
            # p.add(m)
        return p


def tests():
    exp = Exponent(1, 2)
    k = exp.to_key()
    exp_back = Exponent.from_key(k)
    assert exp_back.a == 1
    assert exp_back.b == 2

    m = Mono(2, 1, 3)
    n = Mono(3, 2, 1)
    res = m.mult(n)
    print(res)

    poly = Poly()
    poly.add(m)
    poly.add(n)
    print(poly)

    # p = Poly.from_string("-yx8+9x3-1+y")
    p = Poly.from_string("-yx8")


if __name__ == '__main__':
    sys.exit(tests())
