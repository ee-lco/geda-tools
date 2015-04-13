class CondParser
rule
    \(          { [:LPAR, text] }
    \)          { [:RPAR, text] }
    &&?         { [:AND, text] }
    \|\|?|,     { [:OR, text] }
    [\w\d_-]+   { [:IDENTIFIER, text] }
    \s+

end
