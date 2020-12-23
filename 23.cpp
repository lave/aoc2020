#include <iostream>
#include <list>

struct list_t {
    int val;
    std::list<list_t>::iterator prev;
};

typedef std::list<list_t> cups_t;

std::ostream& operator<<(std::ostream& o, list_t const& it) {
    return o << it.val << '(' << it.prev->val << ')';
}

std::ostream& operator<<(std::ostream& o, cups_t const& l) {
    for (auto i = l.begin(); i != l.end(); ++i) {
        if (i != l.begin())
            o << ", ";
        o << *i;
    }
    return o;
}

cups_t::iterator round(cups_t& l, cups_t::iterator cur) {
    //  make sure there's 3 items after cur
    cups_t::iterator tmp = cur;
    cups_t::iterator copy_end = l.begin();
    for (int i = 0; i < 3; ++i) {
        if (std::next(tmp) == l.end())
            ++copy_end;
        else
            ++tmp;
    }
    l.splice(l.end(), l, l.begin(), copy_end);

    //  find dest
    cups_t::iterator skip_begin = next(cur);
    cups_t::iterator skip_end = skip_begin;
    int n1 = (skip_end++)->val;
    int n2 = (skip_end++)->val;
    int n3 = (skip_end++)->val;

    cups_t::iterator dest = cur->prev;
    while (dest->val == n1 || dest->val == n2 || dest->val == n3)
        dest = dest->prev;

    //  move skip after dest
    l.splice(next(dest), l, skip_begin, skip_end);

    cur = next(cur);
    return cur == l.end() ? l.begin() : cur;
}

int main() {
    int init[] = {7, 8, 9, 4, 6, 5, 1, 2, 3, 10};

    cups_t l;
    for (int i = 0; i < 10; ++i)
        l.push_back({ .val = init[i] });

    for (auto i = l.begin(); i != l.end(); ++i) {
        int prev = i->val == 1 ? 1 : i->val - 1;
        i->prev = std::find_if(l.begin(), l.end(), [=](list_t const& it) -> bool { return it.val == prev; });
    }

    for (int i = 11; i <= 1000000; ++i) {
        l.push_back({ .val = i, .prev = std::prev(l.end()) });
    }
    cups_t::iterator one = std::find_if(l.begin(), l.end(), [](list_t const& it) -> bool { return it.val == 1; });
    one->prev = std::prev(l.end());


    //std::cout << l << std::endl;

    cups_t::iterator cur = l.begin();
    for (int i = 0; i < 10000000; ++i) {
        cur = round(l, cur);
    }

    //std::cout << l << std::endl;

    ++one;
    if (one == l.end())
        one = l.begin();

    long result = one->val;
    ++one;
    if (one == l.end())
        one = l.begin();

    result *= one->val;

    std::cout << result << std::endl;
}
