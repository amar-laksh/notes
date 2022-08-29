#pragma once
#if defined _WIN32 || defined __CYGWIN__
#ifdef BUILDING_EOP
#define EOP_PUBLIC __declspec(dllexport)
#else
#define EOP_PUBLIC __declspec(dllimport)
#endif
#else
#ifdef BUILDING_EOP
#define EOP_PUBLIC __attribute__((visibility("default")))
#else
#define EOP_PUBLIC
#endif
#endif
#include <algorithm>
#include <chrono>
#include <cmath>
#include <functional>
#include <iostream>
#include <memory>
#include <string_view>
#include <thread>
#include <type_traits>
#include <vector>

namespace eop {

template <typename LHS, typename OP> struct LHSlt { LHS lhs_; };
// declare myop as an operator-like construct
enum { typeEquality };
// parse 'lhs <myop' into LHSlt
template <typename LHS>
LHSlt<LHS, decltype(typeEquality)> operator<(const LHS &lhs,
                                             decltype(typeEquality)) {
  return {lhs};
}
// declare (int <myop> int) -> int
template <typename LHSt, typename RHSt>
bool operator>(LHSlt<LHSt, decltype(typeEquality)> lhsof, RHSt rhs) {
  LHSt &lhs = lhsof.lhs_;
  // here comes your actual implementation
  return typeid(lhs) == typeid(rhs) && typeid(rhs) == typeid(lhs);
}

namespace detail {
// To allow ADL with custom begin/end
using std::begin;
using std::end;

template <typename T>
auto is_iterable_impl(int) -> decltype(
    begin(std::declval<T &>()) !=
        end(std::declval<T &>()), // begin/end and operator !=
    ++std::declval<decltype(begin(std::declval<T &>())) &>(), // operator ++
    *begin(std::declval<T &>()),                              // operator*
    std::true_type{});

template <typename T> std::false_type is_iterable_impl(...);

} // namespace detail

template <typename T>
using is_iterable = decltype(detail::is_iterable_impl<T>(0));

class EOP_PUBLIC Eop {

public:
  template <typename L, typename R>
  bool Implication(L l, R r, bool implication) {
    constexpr bool LisIterable = is_iterable<decltype(l)>::value;
    constexpr bool RisIterable = is_iterable<decltype(r)>::value;
    if constexpr (LisIterable) {
      for (auto el : l) {
        Implication(el, r, implication);
      }
    }
    if constexpr (RisIterable) {
      for (auto el : r) {
        Implication(l, el, implication);
      }
    } else {
      if (implication) {
        return true;
      }
    }
    return false;
  }

  template <typename L, typename R> bool RepEq(L l, R r) {
    bool imp = (l == r);
    return Implication(l, r, imp);
  }

  template <typename L, typename R> bool RepEqImpliesEq(L l, R r) {
    bool imp = (!(l == r) || (l<typeEquality> r));
    return Implication(l, r, imp);
  }

  template <typename L, typename R> bool EqImpliesRepEq(L l, R r) {
    bool imp = (!(l<typeEquality> r) || (l == r));
    return Implication(l, r, imp);
  }

  template <typename L, typename R> void ValueTypeIs(const L &l, const R &r) {
    bool repImplieseqHolds = RepEqImpliesEq(l, r);
    bool eqImpliesrepHolds = EqImpliesRepEq(l, r);
    auto and_s = "";
    if (!repImplieseqHolds || !eqImpliesrepHolds) {
      std::cout << "Value-types are ";
    }
    if (!repImplieseqHolds && !eqImpliesrepHolds) {
      and_s = " and ";
    }
    std::cout << (!repImplieseqHolds ? "ambigious" : "") << and_s
              << (!eqImpliesrepHolds ? "not uniquely represented" : "") << "\n";
  }

  template <typename T> constexpr auto type_name() {
    std::string_view name, prefix, suffix;
#ifdef __clang__
    name = __PRETTY_FUNCTION__;
    prefix = "auto type_name() [T = ";
    suffix = "]";
#elif defined(__GNUC__)
    name = __PRETTY_FUNCTION__;
    prefix = "constexpr auto eop::Eop::type_name() [with T = ";
    suffix = "]";
#elif defined(_MSC_VER)
    name = __FUNCSIG__;
    prefix = "auto __cdecl type_name<";
    suffix = ">(void)";
#endif
    name.remove_prefix(prefix.size());
    name.remove_suffix(suffix.size());
    return name;
  }

private:
};
} // namespace eop
