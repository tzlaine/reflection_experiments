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

template<typename T>
consteval std::meta::info make_const_pointer_type(
    std::meta::info initial_pointer_type, std::meta::info value_type)
{
    if (initial_pointer_type != std::meta::info()) {
        if (!type_is_pointer(initial_pointer_type))
            return initial_pointer_type;
        else
            return type_add_pointer(type_add_const(type_remove_pointer(initial_pointer_type)));
    }
    if (^^T::iterator_concept == ^^std::output_iterator_tag)
        return ^^void;
    else
        return type_add_pointer(type_add_const(value_type));
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

#if 0 // TODO: This is currently pretty unusable due to an EDG bug realted to
      // list_builder concatenation.

#if 0 // TODO: (EDG) A previous version of this function looked like this.  The
      // line defining T produced the error, "error: expression must have a 
      // constant value".  Is this correct, or is it a compiler bug?
consteval void iterator_interface_from(std::meta::info t_i, std::string_view mutable_name)
{
    using T = typename[:t_i:];
}
#endif

template<typename T>
consteval void iterator_interface_from(std::string_view mutable_name)
{
    static_assert(requires { typename T::iterator_concept; });
    static_assert(!requires { typename T::iterator_category; });

    constexpr bool adaptor = requires (T t, T const ct) {
        stl_interface_access::base(t);
        stl_interface_access::base(ct);
    };
    // TODO: add support for adaptation, or add a function that does adaptation.
    static_assert(!adaptor, "This implementation does handle the iterator adaptation case.");

    using namespace std::literals;

    // TODO: (EDG) Adding constexpr here causes a failure when injecting below.
    std::meta::info t_i = ^^T;

    std::meta::list_builder mutable_members;
    std::meta::list_builder const_members;

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
    constexpr std::meta::info const_reference_type_i =
        initial_reference_type_i == std::meta::info() ?
        type_add_lvalue_reference(type_add_const(initial_value_type_i)) :
        type_add_lvalue_reference(type_add_const(type_remove_reference(initial_reference_type_i)));
    // Workaround; same semantics as the code above.
    constexpr std::meta::info pointer_type_i =
        make_pointer_type<T>(initial_pointer_type_i, value_type_i);
    constexpr std::meta::info const_pointer_type_i =
        make_const_pointer_type<T>(initial_pointer_type_i, value_type_i);
    constexpr std::meta::info difference_type_i =
        initial_reference_type_i == std::meta::info() ?
        ^^std::ptrdiff_t : initial_difference_type_i;

#if 0 // TODO
    // open up public section
    queue_injection(^^{public:});
#endif

    // TODO: (EDG) These type alias declarations need public:, but cannot use it rn
    // due what seems to be a compiler bug.

    // TODO: Changes these to add tokens to {mutable,const}_members, for
    // injection into the two classes at the end.

    bool const pointer_is_void = pointer_type_i == ^^void;
    bool const make_const_iter = !pointer_is_void && type_is_reference(reference_type_i); // TODO: Provide a flag to disable this.

    mutable_members += ^^{using iterator_concept = [:\(^^T::iterator_concept):];};
    if (make_const_iter)
        const_members += ^^{using iterator_concept = [:\(^^T::iterator_concept):];};

#if 0
    // inject iterator_category
    if (^^T::iterator_concept != ^^std::input_iterator_tag) {
        if (!type_is_reference(reference_type_i)) {
            mutable_members += ^^{using iterator_category = std::input_iterator_tag;};
        } else if (^^T::iterator_concept == ^^std::contiguous_iterator_tag) {
            mutable_members += ^^{using iterator_category = std::random_access_iterator_tag;};
            if (make_const_iter)
                const_members += ^^{using iterator_category = std::random_access_iterator_tag;};
        } else {
            // TODO: (EDG) Why can't I just write "= iterator_concept" below?
            mutable_members += ^^{using iterator_category = [:\(^^T::iterator_concept):];};
            if (make_const_iter)
                const_members += ^^{using iterator_category = [:\(^^T::iterator_concept):];};
        }
    }
#else
    // TODO: (EDG) This causes a build failure; the error is:
    //  /opt/compiler-explorer/edg-gcc-13-experimental-reflection/base/include_cpp_exp/experimental/meta.stdh", line 443: error: expected a declaration
    //  info tokens = ^{}, separator = ^{,};
    //                                   ^.
    mutable_members += ^^{using iterator_category = [:\(^^T::iterator_concept):];};
#endif

#if 0
    // inject missing types
    mutable_members += ^^{using value_type = [:\(value_type_i):];};
    if (make_const_iter)
        const_members += ^^{using value_type = [:\(value_type_i):];};
    mutable_members += ^^{using reference = [:\(reference_type_i):];};
    if (make_const_iter)
        const_members += ^^{using reference = [:\(const_reference_type_i):];};
    mutable_members += ^^{using pointer = [:\(pointer_type_i):];};
    if (make_const_iter)
        const_members += ^^{using pointer = [:\(const_pointer_type_i):];};
    mutable_members += ^^{using difference_type = [:\(difference_type_i):];};
    if (make_const_iter)
        const_members += ^^{using difference_type = [:\(difference_type_i):];};
#endif

#if 0
    // TODO: (EDG) Making these constexpr ICEs EDG.
    using iterator_concept = typename T::iterator_concept;
    bool const contiguous = ^^iterator_concept == ^^std::contiguous_iterator_tag;
    bool const random_access = ^^iterator_concept == ^^std::random_access_iterator_tag;
    bool const bidirectional = ^^iterator_concept == ^^std::bidirectional_iterator_tag;
    bool const forward = ^^iterator_concept == ^^std::forward_iterator_tag;
    bool const input = ^^iterator_concept == ^^std::input_iterator_tag;
    bool const output = ^^iterator_concept == ^^std::output_iterator_tag;

    assert(requires (T t) { *t; });
    if (contiguous || random_access) {
        assert(requires (T t, [:difference_type_i:] n) { t += n; });
        assert(requires (T t) { t - t; });
    } else {
        if (!output)
            assert(requires (T t) { t == t; });
        assert(requires (T t) { ++t; });
        if (bidirectional)
            assert(requires (T t) { --t; });
    }

    constexpr bool literal_type = requires { make_constexpr<T>(); };
    std::meta::list_builder constexpr_tok;
    if constexpr (literal_type)
        constexpr_tok += ^^{constexpr};

    if (!pointer_is_void && type_is_reference(reference_type_i)) {
        mutable_members += ^^{
            constexpr auto operator->(this auto&& self) {
                return std::addressof(*self);
            }
        };
    }

    if (!(contiguous || random_access)) {
        mutable_members += ^^{
            constexpr auto operator++(this auto& self, int) {
                auto retval = self;
                ++self;
                return retval;
            }
        };
        if (bidirectional) {
            mutable_members += ^^{
                constexpr auto operator--(this auto& self, int) {
                    auto retval = self;
                    --self;
                    return retval;
                }
            };
        }
    } else {
        mutable_members += ^^{
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
        };
    }
#endif

    // TODO: Get the type name of T in there instead of hardcpding "impl".
    queue_injection(^^{
        struct \id(mutable_name) {
            \tokens(mutable_members)
        private:
            impl impl_;
        };
    });
    if (make_const_iter) {
        queue_injection(^^{
            struct \id("const_"sv, mutable_name) {
                \tokens(const_members)
            private:
                impl impl_;
            };
        });
    }
}

struct impl
{
    using iterator_concept = std::random_access_iterator_tag;
    using value_type = int;

    impl() {}
    impl(int * it) : it_(it) {}

    int & operator*() const { return *it_; }
    impl & operator+=(std::ptrdiff_t i)
    {
        it_ += i;
        return *this;
    }
    friend std::ptrdiff_t operator-(impl lhs, impl rhs) noexcept
    {
        return lhs.it_ - rhs.it_;
    }

private:
    int * it_;
};

consteval {
    iterator_interface_from<impl>("random_access_iter");
}
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
