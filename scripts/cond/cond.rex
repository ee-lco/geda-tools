class CondParser
rule
    \(          { [:LPAR, text] }
    \)          { [:RPAR, text] }
    &&?         { [:AND, text] }
    \|\|?|,     { [:OR, text] }
    [\w\d_-]+   { [:IDENTIFIER, text] }
    \s+

inner
    def tokenize(text)
        scan_str(text)
        tokens = []
        while token = next_token
            tokens << token
        end
        tokens
    end
end
