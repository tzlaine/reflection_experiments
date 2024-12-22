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

// TODO: This works fine in the static_assert in main(), but not inside
// of inject_iterator_interface().
template<typename T>
consteval bool has_member_function(std::string_view name)
{
    for (auto m : members_of(^^T)) {
        if (is_function(m) && identifier_of(m) == name)
            return true;
    }
    return false;
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
concept __uses_iter_iface = std::same_as<typename T::__iter_iface_tag, void>;

// TODO: there should probably be a proxy version of this too.
template<typename T>
consteval void inject_iterator_interface() {
    static_assert(requires { typename T::iterator_concept; });
    static_assert(!requires { typename T::iterator_category; });

    constexpr bool adaptor = requires (T t, T const ct) {
        stl_interface_access::base(t);
        stl_interface_access::base(ct);
    };

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
#if 0 // TODO: Crashes EDG.
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

    queue_injection(^^{public: using __iter_iface_tag = void;});

    // TODO: These type alias declarations need public:, but cannot use it rn
    // due what seems to be a compiler bug.

    // inject iterator_category
    if (^^T::iterator_concept != ^^std::input_iterator_tag) {
        if (!type_is_reference(reference_type_i)) {
            queue_injection(^^{using iterator_category = std::input_iterator_tag;});
        } else if (^^T::iterator_concept == ^^std::contiguous_iterator_tag) {
            queue_injection(^^{using iterator_category = std::random_access_iterator_tag;});
        } else {
            // TODO: Why can't I just write "= iterator_concept" below?
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

    using iterator_concept = typename T::iterator_concept;
    bool const contiguous = ^^iterator_concept == ^^std::contiguous_iterator_tag;
    bool const random_access = ^^iterator_concept == ^^std::random_access_iterator_tag;
    bool const bidirectional = ^^iterator_concept == ^^std::bidirectional_iterator_tag;
    bool const forward = ^^iterator_concept == ^^std::forward_iterator_tag;
    bool const input = ^^iterator_concept == ^^std::input_iterator_tag;
    bool const output = ^^iterator_concept == ^^std::output_iterator_tag;

    // TODO: Check the availability of members if T that are required for each category above?

    if (contiguous || random_access) {
        queue_injection(^^{
            public: [:\(t_i):] & operator++() {
                *this += 1;
                return *this;
            }
            public: [:\(t_i):] operator++(int) {
                auto tmp = *this;
                ++*this;
                return tmp;
            }
        });
    } else {
        // this should give us a reflection of the enclosing class, but there is currently
        // a bug in EDG, so while it is intended to work - right now you
        // have to pass T explicitly
        // auto T = std::meta::nearest_class_or_namespace();
        queue_injection(^^{
            public: [:\(t_i):] operator++(int) {
                auto tmp = *this;
                ++*this;
                return tmp;
            }
        });
    }
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
    // TODO: Try it with member operator- too.
    friend std::ptrdiff_t operator-(
        basic_random_access_iter lhs, basic_random_access_iter rhs) noexcept
    {
        return lhs.it_ - rhs.it_;
    }

    consteval {
        inject_iterator_interface<basic_random_access_iter>();
    }

private:
    int * it_;
};

struct no_default_ctor {
    explicit no_default_ctor(int) {}
    void func() {}
};

int main()
{
    constexpr std::meta::info const_int_i = ^^const int;
    constexpr std::meta::info int_i = type_remove_const(const_int_i);
    typename[:int_i:] x = 42;
    x = 13;

    std::cout << x << "\n";

    static_assert(has_member_function<no_default_ctor>("func"));
    static_assert(!has_member_function<no_default_ctor>("funk"));

#if 0
    static_assert(is_constructible_type(^^basic_random_access_iter, {}));
    static_assert(!is_constructible_type(^^no_default_ctor, {}));
#endif
}
