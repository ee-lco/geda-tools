class ExprParser
rule
            \$\{        { state = :VAR; [:LVAR, text] }
    :VAR    \}          { state = nil;  [:RVAR, text] }
    :VAR    [\w\d_-]+   {               [:IDENTIFIER, text] }
            [^$]+       {               [:PASS, text] }
            \$          {               [:PASS, text] }

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
