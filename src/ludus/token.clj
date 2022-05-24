(ns ludus.token)

(defn token
  [type text literal line start]
   {::type type
    ::lexeme text
    ::literal literal
    ::line line
    ::start start})
