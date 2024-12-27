
#include <cassert>
#include <tuple>
#include <experimental/meta>
#include <iostream>
#include <algorithm>
#include <ranges>
#include <type_traits>


template<typename T>
using get_value_type = typename T::value_type;
template<typename T>
using get_reference_type = typename T::reference_type;
template<typename T>
using get_pointer_type = typename T::pointer_type;
template<typename T>
using get_difference_type = typename T::difference_type;


template<typename T, template<typename> typename Getter>
consteval std::meta::info reflect_optional_nested_type()
{
    if constexpr (requires {typename Getter<T>;}) {
        return ^^Getter<T>;
    } else {
        return std::meta::info();
    }
}

template<typename T>
consteval std::meta::info make_pointer_type(
    std::meta::info initial_pointer_type, std::meta::info value_type)
{
    if (initial_pointer_type != std::meta::info())
        return initial_pointer_type;
    if (^^T::iterator_concept == ^^std::output_iterator_tag)
        return ^^void;
    else
        return type_add_pointer(value_type);
}

struct stl_interface_access
{
    template<typename T>
    static constexpr auto base(T & t) noexcept -> decltype(t.base_reference())
    {
        return t.base_reference();
    }
    template<typename T>
    static constexpr auto base(T const & t) noexcept -> decltype(t.base_reference())
    {
        return t.base_reference();
    }
};

template<typename T>
consteval T make_constexpr();

// TODO: there should probably be a proxy version of this too.
template<typename T>
consteval void iterator_interface_inline() {
    // this should give us a reflection of the enclosing class, but there is currently
    // a bug in EDG, so while it is intended to work - right now you
    // have to pass T explicitly
    // auto T = std::meta::nearest_class_or_namespace();

    static_assert(requires { typename T::iterator_concept; });
    static_assert(!requires { typename T::iterator_category; });

    constexpr bool adaptor = requires (T t, T const ct) {
        stl_interface_access::base(t);
        stl_interface_access::base(ct);
    };
    // TODO: add support for adaptation, or add a function that does adaptation.
    static_assert(!adaptor, "This implementation does handle the iterator adaptation case.");

    // TODO: (EDG) Adding constexpr here causes a failure when injecting below.
    std::meta::info t_i = ^^T;

    constexpr std::meta::info initial_value_type_i =
        reflect_optional_nested_type<T, get_value_type>();
    constexpr std::meta::info initial_reference_type_i =
        reflect_optional_nested_type<T, get_reference_type>();
    constexpr std::meta::info initial_pointer_type_i =
        reflect_optional_nested_type<T, get_pointer_type>();
    constexpr std::meta::info initial_difference_type_i =
        reflect_optional_nested_type<T, get_difference_type>();

    static_assert(initial_value_type_i == std::meta::info() ||
                  initial_value_type_i == type_remove_cv(initial_value_type_i),
                  "T::value_type must not be const or volatile");

    static_assert(initial_value_type_i != std::meta::info() ||
                  initial_reference_type_i != std::meta::info(),
                  "You must define at least one of T::value_type and T::reference_type");

    constexpr std::meta::info value_type_i =
        initial_value_type_i == std::meta::info() ?
        type_remove_cvref(initial_reference_type_i) : initial_value_type_i;
    constexpr std::meta::info reference_type_i =
        initial_reference_type_i == std::meta::info() ?
        type_add_lvalue_reference(initial_value_type_i) : initial_reference_type_i;
#if 0 // TODO: (EDG) Crashes EDG.
    constexpr std::meta::info pointer_type_i =
        initial_pointer_type_i == std::meta::info() ?
        (^^T::iterator_concept == ^^std::output_iterator_tag ?
         ^^void : type_add_pointer(value_type_i)) :
        initial_pointer_type_i;
#else
    // Workaround; same semantics as the code above.
    constexpr std::meta::info pointer_type_i =
        make_pointer_type<T>(initial_pointer_type_i, value_type_i);
#endif
    constexpr std::meta::info difference_type_i =
        initial_reference_type_i == std::meta::info() ?
        ^^std::ptrdiff_t : initial_difference_type_i;

#if 0 // TODO
    // open up public section
    queue_injection(^^{public:});
#endif

    // TODO: (EDG) These type alias declarations need public:, but cannot use it rn
    // due what seems to be a compiler bug.

    // inject iterator_category
    if (^^T::iterator_concept != ^^std::input_iterator_tag) {
        if (!type_is_reference(reference_type_i)) {
            queue_injection(^^{using iterator_category = std::input_iterator_tag;});
        } else if (^^T::iterator_concept == ^^std::contiguous_iterator_tag) {
            queue_injection(^^{using iterator_category = std::random_access_iterator_tag;});
        } else {
            // TODO: (EDG) Why can't I just write "= iterator_concept" below?
            queue_injection(^^{using iterator_category = [:\(^^T::iterator_concept):];});
        }
    }

    // inject missing types
    if (initial_value_type_i == std::meta::info())
        queue_injection(^^{public: using value_type = [:\(value_type_i):];});
    if (initial_reference_type_i == std::meta::info())
        queue_injection(^^{public: using reference = [:\(reference_type_i):];});
    if (initial_pointer_type_i == std::meta::info())
        queue_injection(^^{public: using pointer = [:\(pointer_type_i):];});
    if (initial_difference_type_i == std::meta::info())
        queue_injection(^^{public: using difference_type = [:\(difference_type_i):];});

    // TODO: (EDG) Making these constexpr ICEs EDG.
    using iterator_concept = typename T::iterator_concept;
    bool const contiguous = ^^iterator_concept == ^^std::contiguous_iterator_tag;
    bool const random_access = ^^iterator_concept == ^^std::random_access_iterator_tag;
    bool const bidirectional = ^^iterator_concept == ^^std::bidirectional_iterator_tag;
    bool const forward = ^^iterator_concept == ^^std::forward_iterator_tag;
    bool const input = ^^iterator_concept == ^^std::input_iterator_tag;
    bool const output = ^^iterator_concept == ^^std::output_iterator_tag;

#if 1 // TODO: Keep these?  Since the free function versions are uncheckable, maybe not?
    assert(requires (T t) { *t; });
    if (contiguous || random_access) {
        assert(requires (T t, [:difference_type_i:] n) { t += n; });
        // TODO: This fails for the free function version of op-.
        // assert(requires (T t) { t - t; });
    } else {
        // TODO: Will fail if implemented as a free function.
        // if (!output)
        //     assert(requires (T t) { t == t; });
        assert(requires (T t) { ++t; });
        if (bidirectional)
            assert(requires (T t) { --t; });
    }
#endif

    bool const pointer_is_void = pointer_type_i == ^^void;

    constexpr bool literal_type = requires { make_constexpr<T>(); };
    std::meta::list_builder constexpr_tok;
    if constexpr (literal_type)
        constexpr_tok += ^^{constexpr};

    if (!pointer_is_void && type_is_reference(reference_type_i)) {
        queue_injection(^^{
        public:
            constexpr auto operator->(this auto&& self) {
                return std::addressof(*self);
            }
        });
    }

    if (!(contiguous || random_access)) {
        queue_injection(^^{
            public:
            constexpr auto operator++(this auto& self, int) {
                auto retval = self;
                ++self;
                return retval;
            }
        });
        if (bidirectional) {
            queue_injection(^^{
            public:
                constexpr auto operator--(this auto& self, int) {
                    auto retval = self;
                    --self;
                    return retval;
                }
            });
        }
    } else {
        queue_injection(^^{
        public:
            constexpr decltype(auto) operator[](this auto const& self, difference_type n) {
                auto retval = self;
                retval = retval + n;
                return *retval;
            }
            constexpr decltype(auto) operator++(this auto& self) {
                return self += difference_type(1);
            }
            constexpr auto operator++(this auto& self, int) {
                auto retval = self;
                ++self;
                return retval;
            }
            constexpr decltype(auto) operator--(this auto& self) {
                return self += -difference_type(1);
            }
            constexpr auto operator--(this auto& self, int) {
                auto retval = self;
                --self;
                return retval;
            }
            constexpr decltype(auto) operator-=(this auto& self, difference_type n) {
                return self += -n;
            }
            friend \tokens(constexpr_tok) auto operator+([:\(t_i):] it, difference_type n)
            { return it += n; }
            friend \tokens(constexpr_tok) auto operator+(difference_type n, [:\(t_i):] it)
            { return it += n; }
            friend \tokens(constexpr_tok) auto operator-([:\(t_i):] it, difference_type n)
            { return it -= n; }
            friend \tokens(constexpr_tok) auto operator<=>([:\(t_i):] lhs, [:\(t_i):] rhs) {
                difference_type const diff = lhs - rhs;
                return diff < difference_type(0) ? std::strong_ordering::less :
                    difference_type(0) < diff ? std::strong_ordering::greater :
                    std::strong_ordering::equal;
            }
            friend \tokens(constexpr_tok) auto operator==([:\(t_i):] lhs, [:\(t_i):] rhs) {
                difference_type const diff = rhs - lhs;
                return diff == difference_type(0);
            }
        });
#if 0
    } else {
        // TODO: (EDG) Putting these here breaks the build.  Note that they are
        // identical to two of the injections above.  I put them at the top, and
        // inverted the conditional expression, (it was originally "contiguous || random_access"),
        // and changed the contiguous/RA case to be the else-case; that fixed things.
        queue_injection(^^{
            public:
            constexpr auto operator++(this auto& self, int) {
                auto retval = self;
                ++self;
                return retval;
            }
        });
        if (bidirectional) {
            queue_injection(^^{
            public:
                constexpr auto operator--(this auto& self, int) {
                    auto retval = self;
                    --self;
                    return retval;
                }
            });
        }
#endif
    }

#if 0 // TODO: If members_of() would return the members *so far*, I would
      // be able to get the ctors from T, and repeat them for the const_
      // version.
    bool make_const_iter = true; // TODO: Provide a flag to disable this.
    if (make_const_iter && type_is_reference(reference_type_i)) {
        std::string_view mutable_name = identifier_of(t_i);
        std::string const_name = "const_";
        const_name.append(mutable_name);
    }
#endif
}

struct basic_random_access_iter
{
    using iterator_concept = std::random_access_iterator_tag;
    using value_type = int;

    basic_random_access_iter() {}
    basic_random_access_iter(int * it) : it_(it) {}

    int & operator*() const { return *it_; }
    basic_random_access_iter & operator+=(std::ptrdiff_t i)
    {
        it_ += i;
        return *this;
    }
    friend std::ptrdiff_t operator-(
        basic_random_access_iter lhs, basic_random_access_iter rhs) noexcept
    {
        return lhs.it_ - rhs.it_;
    }

    consteval {
        iterator_interface_inline<basic_random_access_iter>();
        // TODO: iterator_interface_inline();
    }

private:
    int * it_;
};

#if 0 // TODO
struct const_basic_random_access_iter
{
    using iterator_concept = std::random_access_iterator_tag;
    using value_type = int;

    const_basic_random_access_iter() {}
    const_basic_random_access_iter(int * it) : it_(it) {}

    const int & operator*() const { return *it_; }
    const_basic_random_access_iter & operator+=(std::ptrdiff_t i)
    {
        it_ += i;
        return *this;
    }
    friend std::ptrdiff_t operator-(
        const_basic_random_access_iter lhs, const_basic_random_access_iter rhs) noexcept
    {
        return lhs.it_ - rhs.it_;
    }

    // Other members injected by iterator_interface_inline(); above.

private:
    basic_random_access_iter it_;
};
#endif

struct no_default_ctor {
    explicit no_default_ctor(int) {}
    void func() {}
};

#define CHECK(expr) do {                                      \
  if (!(expr)) std::cout << "FAILED check " << #expr << "\n"; \
} while(false)

int main()
{
    int ints[3] = {0, 1, 2};
    basic_random_access_iter first(ints);
    basic_random_access_iter last(ints + 3);

    std::sort(first, last);
    std::ranges::sort(first, last);

    CHECK(first < last);
    CHECK(first <= last);
    CHECK(!(first > last));
    CHECK(!(first >= last));
    CHECK(!(first == last));
    CHECK(first != last);

    CHECK(first[0] == 0);
    CHECK(first[1] == 1);
    CHECK(first[2] == 2);

    auto it = first;
    ++it;
    ++it;
    ++it;
    CHECK(it == last);
}
