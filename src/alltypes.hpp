#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <utility>

#define UNIQUE_3(a, b) a##b
#define UNIQUE_2(a,b) UNIQUE_3(a, b)
#define UNIQUE(str) UNIQUE_2(str, __COUNTER__)

void __stop_everything(const char* file, const char* function, int line,
                       const char* message1, const char* message2 = 0) {
  printf("#############################################\n");
  printf("# %s\n", message1);
  if (message2) {
    printf("# %s\n", message2);
  }
  printf("# (%s:%s:%d)\n", file, function, line);
  printf("#############################################\n");
  exit(1);
}

void __print_warning(const char* file, const char* function, int line,
                     const char* message) {
  printf("⚠ WARNING: %s ", message);
  printf("(file:%s func:%s line:%d)\n", file, function, line);
}

#ifdef CRUD_SLOW
#define assert(Cond)                                                           \
  ((Cond) ? ((void)0)                                                          \
          : (__stop_everything(__FILE__, __func__, __LINE__,                   \
                               "Assertation failed: " #Cond)))
#define assert_m(Cond, Msg)                                                    \
  ((Cond) ? ((void)0)                                                          \
          : (__stop_everything(__FILE__, __func__, __LINE__,                   \
                               "Assertation failed: " #Cond, Msg)))
#define unreachable(...)                                                       \
  __stop_everything(__FILE__, __func__, __LINE__, "Reached unreachable code.")
#define unimplemented(...)                                                     \
  __stop_everything(__FILE__, __func__, __LINE__, "NOT IMPLEMENTED !!!!!!")
#define warn(Msg) __print_warning(__FILE__, __func__, __LINE__, Msg)
#else
#define assert(...)
#define unreachable(...)
#define unimplemented(...) NOT IMPLEMENTED !!!!!!
#define warn(...)
#endif

namespace _scope_guards {
enum class ScopeGuardOnExit {};

template <typename F> struct ScopeGuard {
  F fn;
  ScopeGuard(F _fn) : fn(_fn){};
  ~ScopeGuard() { fn(); }
};

template <typename F> ScopeGuard<F> operator+(ScopeGuardOnExit, F&& fn) {
  return ScopeGuard<F>(std::forward<F>(fn));
}
} // namespace _scope_guards

// TODO: why aint this work? for why does this break???? big questions
#define defer                                                                  \
  auto UNIQUE(defer_) = _scope_guards::ScopeGuardOnExit() + [&]()

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

static inline void crud_error(const char* fmt,...) {
  va_list va;
  va_start(va, fmt);

  vfprintf(stderr, fmt, va);
  exit(65);
}

#define Max(a, b) (((a) < (b)) ? (b) : (a))

static inline bool is_digit(char c) {
  return (c >= '0' && c <= '9');
}
static inline bool is_alpha(char c) {
  return ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'));
}

static inline int string_length(const char* str) {
  int i = 0;
  while (str[i]) i++;
  return i;
}

static inline int string_length(char* str) {
  int i = 0;
  while (str[i]) i++;
  return i;
}

static inline bool string_match(char* a, const char* b) {
  int i = 0;
  while (a[i] && b[i]) {
    if (a[i] != b[i]) return false;
    i++;
  }
  return true;
}
static inline bool string_match(char* a, const char* b, const size_t l) {
  for (size_t i = 0; i < l; i++)
    if (a[i] != b[i]) return false;
  return true;
}

static inline int string_atoi(char* str) {
  int sign;
  if (*str == '-') {
    sign = -1;
    str += 1;
  } else {
    sign = 1;
  }

  int i = string_length(str) - 1;
  int dec = 1;
  int ret = 0;
  while (i >= 0) {
    assert(is_digit(str[i]));
    ret += (str[i] - '0') * dec;
    dec *= 10;
    i--;
  }

  return ret * sign;
}

template <typename T>
struct Array {
  const size_t MIN_SIZE = 16;

  T* data = 0;
  size_t count = 0, capacity = 0;

  Array() {}

  Array(size_t size) {
    initialize(size);
  }


  T& operator[](int i) {
    assert(i >= 0 && i <= capacity);
    return data[i];
  }

  void initialize(size_t cap = 0) {
    capacity = cap < MIN_SIZE ? MIN_SIZE : cap;
    data = new T[capacity];
  }

  void push(T item) {
    assert(count < capacity);
    maybe_reallocate(1);
    data[count] = item;
    count += 1;
  };

  T pop() {
    if (count <= 0) return {0};
    count -= 1;
    return data[count];
  };

  T* add(size_t n) {
    maybe_reallocate(n);
    count += n;
    return &data[count - n];
  };

  void clear() {
    count = capacity = 0;
  }

  void free() {
    clear();
    ::free(data);
  }

  T* at(size_t i) {
    return &data[i];
  }

  // void append(Slice<T> slice) {
  //   auto dest = add(slice.length);
  //   for (size_t i = 0; i < slice.length; i++) {
  //     dest[i] = slice.data[i];
  //   }
  // }
  void append(T* ptr, const size_t length) {
    auto dest = add(length);
    for (size_t i = 0; i < length; i++) {
      dest[i] = ptr[i];
    }
  }
  void append(Array<T>* other) {
    const size_t n = other->count;
    auto dest = add(n);
    for (size_t i = 0; i < n; i++) {
      dest[i] = other[i];
    }
  };

  void fast_remove(size_t index) {
    assert(index >= 0 && index < count);
    data[index] = pop();
  }

  void remove(size_t index) {
    assert(index >= 0 && index < count);
    for (;index < count; index++) {
      data[index] = data[index + 1];
    }
    count -= 1;
  }

  void insert(T item, size_t index) {
    assert(index >= 0 && index < count);
    maybe_reallocate(1);
    size_t i = count;
    for (; i > index; i--) {
      data[i] = data[i - 1];
    }
    data[i] = item;
    count += 1;
  }

  T* shift() {
    auto result = data[0];
    remove(0);
    return result;
  }

  T* fast_shift() {
    auto result = data[0];
    fast_remove(0);
    return result;
  }

  void unshift(T item) {
    insert(item, 0);
  }

  void maybe_reallocate(size_t n) {
    size_t size_needed = count + n;
    if (size_needed >= capacity) {
      size_t new_cap = capacity;
      while (new_cap <= size_needed) new_cap *= 2;
      resize(new_cap);
    }
  }

  size_t size_in_bytes() {
    return count * sizeof(T);
  }

  void resize(size_t new_cap) {
    if (new_cap <= capacity) return;
    T* old_data = data;
    data = new T[new_cap];
    assert(data);
    memcpy(data, old_data, size_in_bytes());
    delete old_data;
    capacity = new_cap;
  }
};

// using Buffer = Array<u8>;
struct Buffer : Array<u8> {
  void write(const char* fmt, ...) {
    va_list vl;


    // vsprintf();
  }
};

template <typename K, typename V>
struct HashMap {
  static const size_t INITIAL_CAP = 16;
  enum BucketState {
    EMPTY,
    FILLED,
    TOMBSTONE
  };

  struct Bucket {
    size_t hash: 63;
    size_t state: 2;
  };

  struct KeyVal {
    K key;
    V value;
  };

  Array<KeyVal> keyvals;
  Array<Bucket> buckets;
  size_t count;


  static size_t internal_hash(u8* data, const size_t length) {
    // FNV hash
    size_t r = 2166136261;
    for (size_t i = 0; i < length; i++) {
        r ^= data[i];
        r *= 16777619;
    }
    return r;
  }

  size_t hash(K key) {
    return internal_hash((u8*)key, sizeof(K));
  }

  HashMap() : count(0) {
    keyvals.initialize();
    buckets.initialize();
  };

  void reset() {
    keyvals.clear();
    keyvals.resize(INITIAL_CAP);
    buckets.clear();
    buckets.resize(INITIAL_CAP);
  }

  void insert(K,V);
  void remove(K);
  V* get(K);
  void reserve(size_t new_count) {
    rehash(new_count * 3 / 2);
  }

  void rehash(size_t new_count);
};


enum Token_Type {
  SEMICOLON, DOT, COMMA,

  COLON, COLON_EQUALS, COLON_COLON,
  SLASH, SLASH_EQUALS,
  STAR, STAR_EQUALS,
  PLUS, PLUS_EQUALS,
  MINUS, MINUS_EQUALS,
  BANG, BANG_EQUALS,
  EQUAL, EQUAL_EQUAL,
  GREATER, GREATER_EQUAL,
  LESS, LESS_EQUAL,

  L_PAREN, R_PAREN,
  L_BRACKET, R_BRACKET,
  L_BRACE, R_BRACE,

  // literals
  IDENTIFIER, NUMBER, STRING,

  // keywords
  AND, OR,
  IF, ELSE,
  WHILE, FOR,
  TRUE, FALSE,
  STRUCT, DEF,
  // UNION,
  // ENUM,
  // SWITCH,

  LINE_COMMENT,
  BLOCK_COMMENT,

  END_OF_FILE
};

const char* token_type_to_string(Token_Type t) {
#define Case(N) case N: return #N
  switch (t) {
    Case(DOT); Case(COMMA); Case(SEMICOLON);

    Case(COLON);   Case(COLON_EQUALS);  Case(COLON_COLON);
    Case(SLASH);   Case(SLASH_EQUALS);
    Case(STAR);    Case(STAR_EQUALS);
    Case(PLUS);    Case(PLUS_EQUALS);
    Case(MINUS);   Case(MINUS_EQUALS);
    Case(BANG);    Case(BANG_EQUALS);
    Case(EQUAL);   Case(EQUAL_EQUAL);
    Case(GREATER); Case(GREATER_EQUAL);
    Case(LESS);    Case(LESS_EQUAL);

    Case(L_PAREN);   Case(R_PAREN);
    Case(L_BRACE);   Case(R_BRACE);
    Case(L_BRACKET); Case(R_BRACKET);

    Case(IDENTIFIER); Case(NUMBER); Case(STRING);
    Case(LINE_COMMENT);

    Case(BLOCK_COMMENT);

    // keywords
    Case(AND);    Case(OR);
    Case(IF);     Case(ELSE);
    Case(WHILE);  Case(FOR);
    Case(TRUE);   Case(FALSE);
    Case(STRUCT); Case(DEF);

    Case(END_OF_FILE);

    default: unreachable();
  }
  return "(invalid)";
#undef Case
}

struct Keyword {
  const char* lexeme;
  const Token_Type type;
};

Keyword keyword_table[] = {
  { "or", OR },
  { "and", AND },
  { "if", IF },
  { "else", ELSE },
  { "while", WHILE },
  { "for", FOR },
  { "true", TRUE },
  { "false", FALSE },
  { "struct", STRUCT },
  { "def", DEF },
  { 0 } // sentinel
};

Token_Type get_keyword_type(char * str, size_t str_l) {
  Keyword* k = keyword_table;
  while (k->lexeme) {
    if (string_match(str, k->lexeme, str_l)) return k->type;
    k++;
  }
  return IDENTIFIER; // regular old identifier
}

struct Token {
  Token_Type type;
  size_t start, end, line;
};

struct Scanner {
  u8* reader = nullptr; // TODO: why do we steal the pointer from the source?
  size_t start = 0, current = 0, line = 0, end = 0;
  Buffer* source;
  Array<Token> tokens;

  Scanner(Buffer* b) {
    source = b;
    reader = b->data;
    end = b->count;
    tokens.initialize();
  }

  void print_tokens() {
    char buffer[1024];

    for (int i = 0; i < tokens.count; i++) {
      auto token = tokens[i];
      if (token.type != END_OF_FILE) {
        auto len = token.end - token.start;
        assert(len < 1024);
        memcpy(buffer, &source->data[token.start], len);
        buffer[len] = '\0';
      } else {
        buffer[0] = '\0';
      }

      printf("Token { type: %s, lexeme: \"%s\", line: %lu }\n",
        token_type_to_string(token.type), buffer, token.line);
    }
  }

  void free() { tokens.free(); }

  Array<Token> scan_tokens() {
    while (!at_end()) {
      start = current;
      scan_token();
    }

    add_token(END_OF_FILE);
    return tokens;
  }

  void scan_token() {
    char c = advance();
    // printf("scanning: '%c'\n", c);
    switch(c) {
      // TODO: single character lexemes tend to end up longer than they should be
      // because of how `add_token` uses start and current
      case '(': add_token(L_PAREN); break;
      case ')': add_token(R_PAREN); break;
      case '{': add_token(L_BRACE); break;
      case '}': add_token(R_BRACE); break;
      case '[': add_token(L_BRACKET); break;
      case ']': add_token(R_BRACKET); break;
      case '.': add_token(DOT); break;
      case ',': add_token(COMMA); break;
      case ';': add_token(SEMICOLON); break;

      // two-character tokens
      case '*': add_token(match('=') ? STAR_EQUALS : STAR); break;
      case '-': add_token(match('=') ? MINUS_EQUALS : MINUS); break;
      case '+': add_token(match('=') ? PLUS_EQUALS : PLUS); break;
      case '!': add_token(match('=') ? BANG_EQUALS : BANG); break;
      case '<': add_token(match('=') ? LESS_EQUAL : LESS); break;
      case '>': add_token(match('=') ? GREATER_EQUAL : GREATER); break;
      case '=': add_token(match('=') ? EQUAL_EQUAL : EQUAL); break;

      case ':': {
        if (match('=')) {
          add_token(COLON_EQUALS);
        } else if (match(':')) {
          add_token(COLON_COLON);
        } else {
          add_token(COLON);
        }
      } break;

      case '/': {
        if (match('/')) {
          while(peek() != '\n' && !at_end()) advance();
          add_token(LINE_COMMENT);
        } else if (match('*')) {
          while (peek() != '*' && !at_end()) {
            if (advance() == '\n') line++;
          }
          advance();

          if (match('/')) {
            add_token(BLOCK_COMMENT);
          } else {
            crud_error("Unterminated block comment on line %lu\n.", line);
          }
        } else {
          add_token(match('=') ? SLASH_EQUALS : SLASH);
        }
      }break;

      case '\n': line++; break;

      case '\r':
      case '\t':
      case ' ':
      case '\0':
      case 127: // ascii DEL
      case 36: // record seperator
        break;

      // literals
      case '"': do_string(); break;
      default:
        if (is_digit(c)) {
          do_number();
        } else if (is_alpha(c)) {
          do_identifier();
        } else {
          crud_error("Unrecognized character: '%c' (%d), line: %lu\n", c, c, line);
        }
    }
  }

  void do_string() {
    while(peek() != '"' && !at_end()) {
      if (peek() == '\n') line++;
      advance();
    };

    if (at_end()) {
      crud_error("Unterminated string at line: %lu\n", line);
      return;
    }

    advance(); // eat the ending '"'

    add_token(STRING);
  };

  void do_number() {
    // TODO: hex, binary, float representations
    while(is_digit(peek())) advance();

    if (peek() == '.' && is_digit(peek_next())) {
      advance();
      while(is_digit(peek())) advance();
    }

    add_token(NUMBER);
  };

  void do_identifier() {
    // TODO: leading sigil support ($, #, @)
    while (is_alpha(peek()) || peek() == '_') advance();
    Token_Type t = get_keyword_type((char*)&source->data[start], current - start);
    add_token(t);
  };

  void add_token(Token_Type type) {
    Token* t = tokens.add(1);
    t->type = type;
    t->start = start;
    t->end = current;
    t->line = line;
  }

  u8 peek() {
    if (at_end()) return '\0';
    return source->data[current];
  }
  u8 peek_next() {
    if (current + 1 >= end) return '\0';
    return source->data[current + 1];
  }
  u8 advance() {
    current += 1;
    return source->data[current - 1];
  }

  bool at_end() { return current >= end; }

  bool match(const char c) {
    if (at_end()) return false;
    if (peek() != c) return false;
    current++;
    return true;
  }
};