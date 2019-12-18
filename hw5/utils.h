#ifndef UTILS_H_
#define UTILS_H_

#include <variant>

// Variant access without checking
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

// Compile-time string class
template <size_t N> struct CString {
  char str[N + 1];
  constexpr CString() : str{} {}
  constexpr CString(const char (&x)[N + 1]) : str{} {
    for (size_t i = 0; i < N; i++) str[i] = x[i];
  }
  operator std::string() const { return std::string(str, N); }
};
template <size_t N> CString(const char (&lit)[N])
  -> CString<N - 1>;

#endif // UTILS_H_
