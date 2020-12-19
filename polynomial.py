class Polynomial:
    def __init__(self, *coefficients):
        if not coefficients:
            self.cf = {}
            self.list_cf = []
            self.list_deg = []
            return

        self.list_deg = []
        self.cf = {}
        if isinstance(coefficients[0], dict):
            self.cf = coefficients[0]
            self.update_list_deg()
            self.list()
            return

        if isinstance(coefficients[0], Polynomial):
            self.cf = coefficients[0].cf
            self.list_cf = coefficients[0].list_cf[:]
            self.list_deg = coefficients[0].list_deg[:]
            return

        if isinstance(coefficients[0], int) or isinstance(coefficients[0], float):
            self.list_cf = list(coefficients)
            self.update_list()
            self.dict()

        if isinstance(coefficients[0], list):
            self.list_cf = coefficients[0]
            self.update_list()
            self.dict()

        self.update_list_deg()

    def __repr__(self):
        return "Polynomial" + " " + str(self.list_cf)

    def __str__(self):
        str_pol = ""
        for k in self.list_deg:
            str_pol = to_str_pol(str_pol, self.cf, k)

        if str_pol == "":
            str_pol = "0"
        return str_pol

    def __eq__(self, other):
        if isinstance(other, Polynomial):
            return self.list_cf == other.list_cf
        if isinstance(other, int) or isinstance(other, float):
            return [other] == self.list_cf
        if other is None:
            return self.list_cf == []

    def __add__(self, other):
        new_pol = Polynomial(self)
        second_pol = Polynomial(other)
        if second_pol is None:
            return new_pol

        if isinstance(second_pol, int):
            new_pol.list_cf = [new_pol.list_cf[0] + second_pol] \
                              + new_pol.list_cf[1:]

        if isinstance(second_pol, Polynomial):
            max_len = max((len(new_pol.list_cf), 1),
                          (len(second_pol.list_cf), 0))
            two_list = (new_pol.list_cf, second_pol.list_cf)

            for k in range(max_len[0] - len(two_list[max_len[1]])):
                two_list[max_len[1]].append(0)
            new_pol.list_cf = [x + y for x, y in
                               zip(new_pol.list_cf, second_pol.list_cf)]

        new_pol.update_list()
        new_pol.dict()
        new_pol.update_list_deg()
        return new_pol

    def __radd__(self, other):
        first_pol = Polynomial(self)
        second_pol = Polynomial(other)
        return first_pol.__add__(second_pol)

    def __neg__(self):
        new_list_cf = self.list_cf[:]
        new_list_cf = [x * (- 1) for x in
                       new_list_cf]
        return Polynomial(new_list_cf)

    def __pos__(self):
        new_pol = Polynomial(self)
        return new_pol

    def __sub__(self, other):
        return self + (-other)

    def __rsub__(self, other):
        return other + (-self)

    def __call__(self, x):
        if isinstance(x, list):
            ans = []
            for k in x:
                ans.append(self(k))
            return ans

        ans = 0
        for k in self:
            if k[0] == 0:
                ans += k[1]
            else:
                ans += k[1] * (x ** k[0])
        return ans

    def list(self):
        self.list_cf = []
        last = 0
        if self.list_deg[len(self.list_deg) - 1] != 0:
            self.list_cf.append(0)
        for k in reversed(self.list_deg):
            for i in range(last, k - 1):
                self.list_cf.append(0)
            self.list_cf.append(self.cf[k])
            last = k
        self.update_list()

    def update_list_deg(self):
        self.list_deg = [*self.cf.keys()]
        self.list_deg.sort(reverse=True)

    def update_list(self):
        zero_len = 0
        for k in reversed(self.list_cf):
            if k != 0:
                break
            zero_len += 1
        list_len = len(self.list_cf)
        if zero_len == list_len:
            self.list_cf = [0]
            return
        self.list_cf = self.list_cf[:(list_len - zero_len)]

    def dict(self):
        self.cf = {k: self.list_cf[k] for k in
                   range(len(self.list_cf))}

    def degree(self):
        for k in self.list_deg:
            if self.cf[k] != 0:
                return k
        return 0

    def der(self, d=1):
        new_pol = Polynomial(self)
        if d == 0:
            return new_pol
        new_cf = {}
        for k in new_pol.cf:
            if k != 0:
                new_cf[k - 1] = new_pol.cf[k] * k
        new_pol.cf = new_cf
        new_pol.update_list_deg()
        new_pol.list()
        return new_pol.der(d - 1)

    def __mul__(self, other):
        new_pol = Polynomial(self)
        if isinstance(other, int):
            new_pol.list_cf = [x * other for x in
                               new_pol.list_cf]

        if isinstance(other, Polynomial):
            new_pol.list_cf = mul_pol(new_pol, other)

        new_pol.update_list()
        new_pol.dict()
        new_pol.update_list_deg()
        return new_pol

    def __rmul__(self, other):
        return self * other

    def __iter__(self):
        self.iter = 0
        return self

    def __next__(self):
        if self.iter > self.degree():
            self.iter = 0
            raise StopIteration
        i = self.iter
        self.iter += 1
        return i, self.list_cf[i]


class QuadraticPolynomial(Polynomial):
    def __init__(self, *coefficients):
        super().__init__(*coefficients)
        try:
            if self.degree() > 2:
                raise DegreeIsTooBigException("DegreeIsTooBigException")
        except NotOddDegreeException as er:
            print(er)

    def solve(self):
        degree = self.degree()
        if degree == 0:
            return []
        if degree == 1:
            return [- self.cf[0] / self.cf[1]]
        delta = self.cf[1] ** 2 - 4 * self.cf[2] * self.cf[0]
        if delta < 0:
            return []
        if delta == 0:
            return [-self.cf[1] / (2 * self.cf[2])]
        x1 = (- self.cf[1] - sqrt(delta, 5)) / (2 * self.cf[2])
        x2 = (- self.cf[1] + sqrt(delta, 5)) / (2 * self.cf[2])
        return [x1, x2]


class NotOddDegreeException(Exception):
    def __init__(self, text):
        self.txt = text


class DegreeIsTooBigException(Exception):
    def __init__(self, text):
        self.txt = text


class RealPolynomial(Polynomial):
    def __init__(self, *coefficients):
        super().__init__(*coefficients)
        try:
            if self.degree() % 2 == 0:
                raise NotOddDegreeException("NotOddDegreeException")
        except NotOddDegreeException as er:
            print(er)

    def find_root(self, **kwargs):
        delta = default_val("delta", kwargs)
        a = default_val("a", kwargs, 0)
        b = default_val("b", kwargs, delta)
        roots = check4root(self, a, b)
        if roots:
            return stair(self, roots[0], roots[1])
        a = shift_seg(a, delta)
        b = shift_seg(b, delta)
        return self.find_root(delta=delta, a=a, b=b)


def to_str_pol(str_pol, cf, k):
    if cf[k] == 0:
        return str_pol
    if cf[k] > 0 and str_pol != "":
        str_pol += " + "
    if cf[k] < 0:
        if str_pol == "":
            str_pol += "-"
        else:
            str_pol += " - "

    abs_cf = abs(cf[k])
    if abs_cf != 1 or k == 0:
        str_pol += str(abs(cf[k]))
    if k != 0 and k != 1:
        str_pol += "x^" + str(k)
    if k == 1:
        str_pol += "x"
    return str_pol


def add_zero(list_cf, n):
    for d in range(len(list_cf), n + 1):
        list_cf.append(0)
    return list_cf


def mul_pol(pol1, pol2):
    new_cf = []
    for n in range(pol1.degree() + pol2.degree() + 1):
        new_cf.append(0)
        first_cf = pol1.list_cf[:n + 1]
        first_cf = add_zero(first_cf, n)
        second_cf = pol2.list_cf[:n + 1]
        second_cf = add_zero(second_cf, n)
        second_cf.reverse()

        for i, j in zip(first_cf, second_cf):
            new_cf[n] += i * j
    return new_cf


def check4root(pol, a, b, gamma=1000, *args):
    if not args:
        check_all = True
    else:
        check_all = args[0]
    left, right = min(a, b), max(a, b)

    left_val = pol(left)
    if left_val == 0:
        return left_val, left_val
    right_val = pol(right)
    if right_val == 0:
        return right_val, right_val

    if sgn(left_val) == sgn(right_val):
        if not check_all:
            return False
        result = strong_check4root(pol, left, right, gamma)
        return result
    return left, right


def strong_check4root(pol, left, right, gamma=1000):
    for k in range(1, gamma):
        delta_seg = abs(right - left)
        if check4root(pol, left + delta_seg / (k + 1),
                      left + delta_seg / k, gamma, False):
            left = left + delta_seg / (k + 1)
            right = left + delta_seg / k
            return left, right
    return False


def stair(pol, left, right, n=1000, *args):
    if left == right:
        return left
    if args:
        if args[0] == left and args[1] == right:
            return right
    n -= 1
    if n == 0:
        return (left + right) / 2
    left, right = min(left, right), max(left, right)
    sign_left = sgn(pol(left))
    sign_centre = sgn(pol((left + right) / 2))
    if sign_centre == sign_left:
        return stair(pol, (left + right) / 2,
                     right, n, left, right)
    return stair(pol, left, (left + right) / 2,
                 n, left, right)


def default_val(x, kwargs, val=1):
    if x in kwargs:
        return kwargs[x]
    return val


def sgn(x):
    if x == 0:
        return 0
    return x / abs(x)


def shift_seg(x, delta):
    return -(abs(x) + delta) * sgn(x)


def sqrt(x, n):
    x = x * 10 ** (2 * n)
    for k in range(int(x / 2)):
        if k ** 2 > x:
            return (k - 1) / (10 ** n)
