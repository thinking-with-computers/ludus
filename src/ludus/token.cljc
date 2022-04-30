(ns ludus.token)

(defn token
  ([type text]
   (token type text nil 1))
  ([type text literal line start]
   {::type type
    ::lexeme text
    ::literal literal
    ::line line
    ::start start}))
