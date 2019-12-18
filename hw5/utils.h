#ifndef UTILS_H_
#define UTILS_H_

#include <variant>

template <class... Types> struct IndexOf_ {
  static constexpr size_t value = 0;
};
template <class T, class Type, class... Types> struct IndexOf_<T, Type, Types...> {
  static constexpr size_t value = IndexOf_<T, Types...>::value + 1;
};
template <class T, class... Types> struct IndexOf_<T, T, Types...> {
  static constexpr size_t value = 0;
};

template <class T, class... Types>
constexpr T& GetNoCheck(std::variant<Types...>& v) noexcept {
  if (v.index() != IndexOf_<T, Types...>::value) __builtin_unreachable();
  return *std::get_if<T>(&v);
}
template <class T, class... Types>
constexpr T&& GetNoCheck(std::variant<Types...>&& v) noexcept {
  if (v.index() != IndexOf_<T, Types...>::value) __builtin_unreachable();
  return *std::get_if<T>(&v);
}
template <class T, class... Types>
constexpr const T& GetNoCheck(const std::variant<Types...>& v) noexcept {
  if (v.index() != IndexOf_<T, Types...>::value) __builtin_unreachable();
  return *std::get_if<T>(&v);
}
template <class T, class... Types>
constexpr const T&& GetNoCheck(const std::variant<Types...>&& v) noexcept {
  if (v.index() != IndexOf_<T, Types...>::value) __builtin_unreachable();
  return *std::get_if<T>(&v);
}
#ifdef NDEBUG
template <class X, class... T> inline auto Get(T&&... args) noexcept
    -> decltype(GetNoCheck<X>(std::forward<T>(args)...)) {
  return GetNoCheck<X>(std::forward<T>(args)...);
}
#else
template <class X, class... T> inline auto Get(T&&... args)
    -> decltype(std::get<X>(std::forward<T>(args)...)) {
  return std::get<X>(std::forward<T>(args)...);
}
#endif

#endif // UTILS_H_
